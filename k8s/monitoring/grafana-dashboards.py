#!/usr/bin/env python

from __future__ import print_function, unicode_literals

import attr
from attr.validators import instance_of

from sys import argv
from io import BytesIO
from json import loads, dumps
from itertools import count

# Satisfy grafanalib
class machinery:
    SourceFileLoader = None
import sys
sys.modules['importlib.machinery'] = machinery

import grafanalib.core as G
from grafanalib._gen import write_dashboard

from twisted.internet.defer import inlineCallbacks
from twisted.internet.task import react

from txkube import network_kubernetes_from_context

X_TIME = G.XAxis(
    name="When",
    mode="time",
)

def refidgen():
    for i in count():
        yield unicode(i)
refid = refidgen()



@attr.s
class Heatmap(object):
    title = attr.ib()
    dataSource = attr.ib()
    targets = attr.ib()
    id = attr.ib(default=None)

    xAxis = attr.ib(default=attr.Factory(G.XAxis), validator=instance_of(G.XAxis))
    # XXX: This isn't a *good* default, rather it's the default Grafana uses.
    yAxis = attr.ib(
        default=attr.Factory(lambda: G.YAxis(format=G.SHORT_FORMAT)))

    span = attr.ib(default=None)

    def to_json_data(self):
        return {
            "title": self.title,
            "datasource": self.dataSource,
            "targets": self.targets,
            "xAxis": self.xAxis,
            'yAxis': self.yAxis,
            "span": self.span,

            "dataFormat": "timeseries",
            "type": "heatmap",
            "heatmap": {},

            "color": {
                "cardColor": "#b4ff00",
                "colorScale": "sqrt",
                "colorScheme": "interpolateOranges",
                "exponent": 0.5,
                "mode": "spectrum"
            },
        }



def cpu_usage(datasource, intervals):
    return G.Graph(
        title="CPU usage",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="percent",
                label="Average",
                min=0,
                max=100,
            ),
            G.YAxis(
                format="percent",
                label="Average",
            ),
        ],
        targets=list(
            G.Target(
                # CPU usage (as a percentage of maximum possible) averaged
                # over a period is given as 100 times the sum (over all
                # containers) of the rate of increase (in seconds) divided by
                # the maximum possible increase (1 second per CPU).
                #
                # The sums are taken from recording rules because recomputing
                # them for every point on the graph for every graph request
                # becomes prohitively expensive.  Only a few specific rates
                # are "recorded" and the ``interval`` parameter must match one
                # of those. :(
                #
                # See prometheus.yaml for the recording rules.
                expr="""
                  100
                * cpu:container_usage_seconds:rate{}
                / cores:machine_cpu:total
                """.format(interval),
                legendFormat="CPU Usage ({} avg)".format(interval),
                refId=next(refid),
            )
            for n, interval in enumerate(intervals),
        ),
    )



def memory_usage(datasource):
    return G.Graph(
        title="Memory Usage",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                # 2 ^ 30 bytes
                format="gbytes",
                label="Memory",
            ),
            G.YAxis(
                show=False,
            ),
        ],
        targets=[
            G.Target(
                expr="""
                sum(machine_memory_bytes) / 2 ^ 30
                """,
                legendFormat="Total Physical Memory",
                refId=next(refid),
            ),
            G.Target(
                expr="""
                rss:container_memory:total / 2 ^ 30
                """,
                legendFormat="Total Container RSS",
                refId=next(refid),
            ),
        ],
    )



def network_usage(datasource):
    return G.Graph(
        title="Network Usage",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                # 2^20 bytes / second
                format="MBs",
                label="Transferred",
            ),
            G.YAxis(
                show=False,
            ),
        ],
        targets=[
            G.Target(
                # Get the rate of data received on the public interface (eth0)
                # for each entire node (id="/") over the last minute.
                expr="""
                receive:container_network_bytes:rate1m / 2 ^ 20
                """,
                legendFormat="receive",
                refId=next(refid),
            ),
            G.Target(
                # And rate of data sent.
                expr="""
                transmit:container_network_bytes:rate1m / 2 ^ 20
                """,
                legendFormat="transmit",
                refId=next(refid),
            ),
        ],
    )



def filesystem_usage(datasource):
    return G.Graph(
        title="Filesystem Usage",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="percent",
            ),
            G.YAxis(
                show=False,
            ),
        ],
        targets=[
            G.Target(
                # Get the proportion used of each filesystem on a volume from
                # a PersistentVolumeClaim on each node of the cluster.  It's
                # hard to figure out the role each filesystem serves from this
                # graph (since all we get is the PVC name).  Better than
                # nothing, though.  Hopefully later we can do better.
                expr="""
                100
                * filesystem_used_bytes{volume=~"pvc-.*"}
                / filesystem_size_bytes{volume=~"pvc-.*"}
                """,
                legendFormat="{{volume}}",
                refId=next(refid),
            ),
        ],
    )


def tahoe_lafs_transfer_rate(datasource, direction):
    return Heatmap(
        title="Tahoe-LAFS Benchmarked {direction} Rate".format(direction=direction),
        dataSource=datasource,

        xAxis=X_TIME,
        yAxis=G.YAxis(
            format="MBs",
            label="Transfer Rate",
        ),

        targets=[
            G.Target(
                expr="""
                tahoe_lafs_roundtrip_benchmark_last_{direction}_bytes_per_second / 1024 / 1024
                """.format(direction=direction),
                legendFormat=direction.title(),
                refId=next(refid),
            ),
        ],
    )


def s4_customer_deployments(datasource):
    return G.Graph(
        title="Customer Deployments",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="none",
                label="Count",
                min=0,
                max=100,
            ),
            G.YAxis(
                show=False,
            ),
        ],

        targets=[
            G.Target(
                expr="""
                s4_deployment_gauge
                """,
                refId=next(refid),
                legendFormat="Total Customer Deployments",
            ),
        ],
    )



def last_convergence(datasource):
    return G.Graph(
        title="Since Last Convergence",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="none",
                label="Period",
            ),
            G.YAxis(
                show=False,
            ),
        ],

        targets=[
            G.Target(
                expr="""
                time()
                - max(
                    s4_last_convergence_succeeded{
                        pod=~"subscription-converger-.*"
                    }
                )
                """,
                refId=next(refid),
                legendFormat="Time Since Last Convergence Success",
            ),
        ],
    )



def unhandled_errors(datasource):
    return G.Graph(
        title="Unhandled Errors",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="none",
                label="Count",
            ),
            G.YAxis(
                show=False,
            ),
        ],

        targets=[
            G.Target(
                expr="""
                sum(s4_unhandled_error_counter)
                """,
                refId=next(refid),
                legendFormat="Total Unhandled Errors",
            ),
        ],
    )



def process_open_fds(datasource):
    return G.Graph(
        title="Open File Descriptors",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                format="none",
                label="Count",
            ),
            G.YAxis(
                show=False,
            ),
        ],

        targets=[
            G.Target(
                expr="""
                process_open_fds{pod=~".+"}
                """,
                refId=next(refid),
                legendFormat="{{pod}}",
            ),
        ],
    )



def dashboard():
    PROMETHEUS = "prometheus"
    return G.Dashboard(
        title="S4",
        rows=[
            G.Row(panels=[
                G.Graph(
                    title="Signups",
                    dataSource=PROMETHEUS,
                    xAxis=X_TIME,
                    yAxes=[
                        G.YAxis(
                            format="none",
                            label="Count",
                        ),
                        G.YAxis(
                            format="none",
                            label="Count",
                        ),
                    ],
                    targets=[
                        G.Target(
                            # Filter down to just the signup pod since that's
                            # the only one where this metric value is
                            # meaningful.  Some other pods report a 0 value
                            # for this metric because they happen to import
                            # the Python code that defines the object
                            # representing it.
                            #
                            # Also, sum over the selected series to account
                            # for pod replacement.
                            expr='sum(wormhole_signup_started{pod=~"s4-signup.*"})',
                            legendFormat="Wormhole Signups Started",
                            refId=next(refid),
                        ),
                        G.Target(
                            expr='sum(wormhole_signup_success{pod=~"s4-signup.*"})',
                            legendFormat="Wormhole Signups Completed",
                            refId=next(refid),
                        ),
                        G.Target(
                            expr='sum(wormhole_signup_failure{pod=~"s4-signup.*"})',
                            legendFormat="Wormhole Signups Failed",
                            refId=next(refid),
                        ),
                    ],
                ),
                G.Graph(
                    title="Usage",
                    dataSource=PROMETHEUS,

                    # Stack the connection graphs on each other, revealing
                    # both a total and a distribution across different grid
                    # router instances.
                    stack=True,
                    tooltip=G.Tooltip(
                        valueType=G.INDIVIDUAL,
                    ),

                    xAxis=X_TIME,
                    yAxes=[
                        G.YAxis(
                            format="none",
                            label="Count",
                        ),
                        G.YAxis(
                            format="none",
                            label="Count",
                        ),
                    ],
                    targets=[
                        G.Target(
                            expr="grid_router_connections",
                            legendFormat="Tahoe-LAFS Connections",
                            refId=next(refid),
                        ),
                    ],
                ),
                last_convergence(PROMETHEUS),
                unhandled_errors(PROMETHEUS),
            ]),
            G.Row(
                title="Cluster",
                panels=[
                    cpu_usage(PROMETHEUS, ["1m", "5m", "10m"]),
                    memory_usage(PROMETHEUS),
                    network_usage(PROMETHEUS),
                    filesystem_usage(PROMETHEUS),
                ],
            ),
            G.Row(panels=[
                tahoe_lafs_transfer_rate(PROMETHEUS, "read"),
                process_open_fds(PROMETHEUS),
            ]),
            G.Row(panels=[
                tahoe_lafs_transfer_rate(PROMETHEUS, "write"),
                s4_customer_deployments(PROMETHEUS),
            ]),
        ],
    ).auto_panel_ids()



def dashboard_to_json_bytes(dashboard):
    io = BytesIO()
    write_dashboard(dashboard, io)
    dashboard_json_bytes = io.getvalue()
    return dashboard_json_bytes



def configmap(model, dashboard):
    return model.v1.ConfigMap(
        metadata={
            "name": "grafana-dashboards",
            "labels": {
                "provider": "LeastAuthority",
                "component": "Monitoring",
                "service": "grafana",
            },
        },
        data={
            "prometheus-datasource.json": dumps({
                'access': 'proxy',
                'basicAuth': False,
                'name': 'prometheus',
                'type': 'prometheus',
                'url': 'http://prometheus/',
                'orgId': 1,
            }).decode("ascii"),
            "s4-dashboard.json": dumps({
                "dashboard": loads(dashboard_to_json_bytes(dashboard)),
                "overwrite": True,
            }).decode("ascii"),
        },
    )


@inlineCallbacks
def main(reactor, context):
    kubernetes = network_kubernetes_from_context(reactor, context)
    client = yield kubernetes.client()
    model = client.model
    cfg = model.iobject_to_raw(configmap(model, dashboard()))
    print(dumps(cfg))


if __name__ == '__main__':
    react(main, argv[1:])

#!/usr/bin/env python

from __future__ import print_function, unicode_literals

from sys import argv
from io import BytesIO

from json import loads, dumps

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
                refId=chr(ord("A") + n),
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
                refId="A",
            ),
            G.Target(
                expr="""
                rss:container_memory:total / 2 ^ 30
                """,
                legendFormat="Total Container RSS",
                refId="B",
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
                refId="A",
            ),
            G.Target(
                # And rate of data sent.
                expr="""
                transmit:container_network_bytes:rate1m / 2 ^ 20
                """,
                legendFormat="transmit",
                refId="B",
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
                refId="A",
            ),
        ],
    )



def tahoe_lafs_transfer_rate(datasource):
    return G.Graph(
        title="Tahoe-LAFS Benchmarked Transfer Rate",
        dataSource=datasource,

        xAxis=X_TIME,
        yAxes=[
            G.YAxis(
                # 2^10 bytes / second
                format="KBs",
                label="Transfer Rate",
            ),
            G.YAxis(
                show=False,
            ),
        ],

        targets=[
            G.Target(
                expr="""
                  tahoe_lafs_roundtrip_benchmark_write_bytes_per_second_sum
                / tahoe_lafs_roundtrip_benchmark_write_bytes_per_second_count
                """,
                legendFormat="upload",
                refId="A",
            ),
            G.Target(
                expr="""
                  tahoe_lafs_roundtrip_benchmark_read_bytes_per_second_sum
                / tahoe_lafs_roundtrip_benchmark_read_bytes_per_second_count
                """,
                legendFormat="download",
                refId="B",
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
                refId="A",
                legendFormat="Total Customer Deployments",
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
                s4_unhandled_error_counter
                """,
                refId="A",
                legendFormat="Total Unhandled Errors",
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
                            expr='wormhole_signup_started{pod=~"s4-signup.*"}',
                            legendFormat="Wormhole Signups Started",
                            refId="A",
                        ),
                        G.Target(
                            expr='wormhole_signup_success{pod=~"s4-signup.*"}',
                            legendFormat="Wormhole Signups Completed",
                            refId="B",
                        ),
                        G.Target(
                            expr='wormhole_signup_failure{pod=~"s4-signup.*"}',
                            legendFormat="Wormhole Signups Failed",
                            refId="C",
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
                            refId="D",
                        ),
                    ],
                ),
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
                tahoe_lafs_transfer_rate(PROMETHEUS),
                s4_customer_deployments(PROMETHEUS),
                unhandled_errors(PROMETHEUS),
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

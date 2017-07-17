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


def dashboard():
    PROMETHEUS = "prometheus"

    return G.Dashboard(
        title="S4",
        rows=[
            G.Row(panels=[
                G.Graph(
                    title="Signups",
                    dataSource=PROMETHEUS,
                    xAxis=G.XAxis(
                        name="When",
                        mode="time",
                    ),
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
            ]),
            G.Row(panels=[
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

                    xAxis=G.XAxis(
                        name="When",
                        mode="time",
                    ),
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
            G.Row(panels=[
                G.SingleStat(
                    title='Current Customer Deployments',
                    dataSource='prometheus',
                    valueName='current',
                    sparkline=G.SparkLine(show=True),
                    targets=[
                        G.Target(
                            expr='s4_deployment_gauge',
                            refId="E",
                        ),
                    ],
                ),
                G.SingleStat(
                    title='Unhandled Errors',
                    dataSource='prometheus',
                    valueName='current',
                    sparkline=G.SparkLine(show=True),
                    targets=[
                        G.Target(
                            expr='s4_unhandled_error_counter',
                            refId="F",
                        ),
                    ],
                ),
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

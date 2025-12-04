#!/usr/bin/env python3
"""
Génère un graphe interactif (HTML) des comptes suivis par les fans de trains
avec NetworkX + Louvain + PyVis.

Usage :
    python cours_5/insta_trains_pyvis.py
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, Iterable, Tuple

import networkx as nx
import pandas as pd
from community import community_louvain
from pyvis.network import Network


def load_edges(json_path: Path) -> pd.DataFrame:
    """Charge le JSON et renvoie une edge list fan -> compte suivi."""
    with json_path.open("r", encoding="utf-8") as f:
        data = json.load(f)

    rows = []
    for idx, accounts in enumerate(data, start=1):
        fan = f"fan_{idx}"
        if not isinstance(accounts, Iterable):
            continue
        for account in accounts:
            username = account.get("username") if isinstance(account, dict) else None
            if username:
                rows.append((fan, username))

    return pd.DataFrame(rows, columns=["from", "to"]).dropna().drop_duplicates()


def build_graph(edges: pd.DataFrame, min_in: int = 10) -> Tuple[nx.Graph, Dict[str, int]]:
    """Construit le graphe filtré + attributs (type, indegree, communauté)."""
    g_dir = nx.DiGraph()
    g_dir.add_edges_from(edges.itertuples(index=False, name=None))

    indeg = dict(g_dir.in_degree())

    # Garde tous les fans + comptes suffisamment suivis
    keep_nodes = {n for n, d in indeg.items() if d >= min_in or n.startswith("fan_")}
    g_sub = nx.Graph()
    g_sub.add_edges_from((u, v) for u, v in g_dir.edges() if u in keep_nodes and v in keep_nodes)

    # Attributs de base
    for node in g_sub.nodes():
        g_sub.nodes[node]["type"] = "Fan" if node.startswith("fan_") else "Compte suivi"
        g_sub.nodes[node]["indegree"] = indeg.get(node, 0)

    if g_sub.number_of_nodes():
        comm = community_louvain.best_partition(g_sub)
        nx.set_node_attributes(g_sub, comm, "community")

    return g_sub, indeg


def render_pyvis(g: nx.Graph, indeg: Dict[str, int], output_html: Path) -> Path:
    """Rendu interactif PyVis."""
    if g.number_of_nodes() == 0:
        raise ValueError("Graphe vide après filtrage (aucun noeud conservé).")

    non_fan_deg = [d for n, d in indeg.items() if not n.startswith("fan_") and n in g]
    label_cutoff = pd.Series(non_fan_deg).quantile(0.95) if non_fan_deg else 0

    palette = [
        "#3B82F6",
        "#F97316",
        "#10B981",
        "#A855F7",
        "#EC4899",
        "#F59E0B",
        "#22D3EE",
        "#94A3B8",
        "#4ADE80",
        "#C084FC",
    ]

    net = Network(height="850px", width="100%", bgcolor="#f9f9f9", directed=False)
    net.barnes_hut()

    for node, data in g.nodes(data=True):
        comm = data.get("community", 0)
        color = palette[comm % len(palette)]
        indegree = data.get("indegree", 0)
        ntype = data.get("type", "Compte suivi")

        label = node if (ntype != "Fan" and indegree >= label_cutoff) else None
        title = f"{ntype}<br>In-degree: {indegree}<br>Communauté: {comm}"
        size = max(4, min(24, 4 + indegree))
        shape = "dot" if ntype == "Compte suivi" else "ellipse"

        net.add_node(
            node,
            label=label or node,
            title=title,
            size=size,
            color=color,
            shape=shape,
        )

    for u, v in g.edges():
        net.add_edge(u, v, color="#b0b0b0", value=1)

    output_html.parent.mkdir(parents=True, exist_ok=True)
    net.save_graph(output_html.as_posix())
    return output_html


def main() -> None:
    base = Path(__file__).resolve().parent
    candidates = [
        base / "data_full" / "ALL_FOLLOWING_DATA.json",
        base.parent / "data_full" / "ALL_FOLLOWING_DATA.json",
    ]
    json_path = next((p for p in candidates if p.exists()), None)
    if not json_path:
        raise FileNotFoundError("Fichier ALL_FOLLOWING_DATA.json introuvable dans data_full/.")

    edges = load_edges(json_path)
    graph, indeg = build_graph(edges, min_in=10)

    output_html = base / "images" / "insta_trains_pyvis.html"
    saved = render_pyvis(graph, indeg, output_html)
    print(f"Graphe exporté vers {saved}")


if __name__ == "__main__":
    main()

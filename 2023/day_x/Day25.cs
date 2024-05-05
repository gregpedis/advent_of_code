using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;

namespace day_x;

internal static class Day25
{
	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day25.txt");
		MakeGraph(rows);
	}

	static void MakeGraph(string[] rows)
	{
		var nodes = new HashSet<string>();
		var edges = new HashSet<Edge>();

		foreach (var row in rows)
		{
			var splitted = row.Split(':');
			var node1 = splitted[0].Trim();
			var right = splitted[1].Trim().Split(' ');

			foreach (var node2 in right)
			{
				edges.Add(new(node1, node2));
				nodes.Add(node1);
				nodes.Add(node2);
			}
		}

		Console.WriteLine("strict graph { ");
		foreach (var edge in edges)
		{
			Console.WriteLine($"{edge.Node1} -- {edge.Node2}");
		}
		Console.WriteLine("}");
	}

	record Edge(string Node1, string Node2);
}

using System.Linq;
using System.Text.Json.Nodes;

namespace day_x;

internal static class Day17
{
	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day17.txt");

		var part1 = Dijkstra(ParseInput1(rows));
		Console.WriteLine($"Total steps: {part1}");
	}

	private static int[][] ParseInput1(string[] rows)
	{
		var res = new List<int[]>();

		foreach (var row in rows)
		{
			res.Add(row.Select(x => x - '0').ToArray());
		}

		return res.ToArray();
	}

	private static int Dijkstra(int[][] points)
	{
		var pq = new PriorityQueue<Point, int>();
		var start = new Point(0, 0);
		pq.Enqueue(start, 0);

		Dictionary<Point, Point?> cameFrom = new();
		Dictionary<Point, int> costSoFar = new();
		cameFrom[start] = null;
		costSoFar[start] = 0;

		while (pq.Count > 0)
		{
			var current = pq.Dequeue();
			foreach (var next in Neighbours(current, points, cameFrom))
			{
				var newCost = costSoFar[current] + points[next.X][next.Y];
				if (!costSoFar.TryGetValue(next, out var oldCost) || newCost < oldCost)
				{
					costSoFar[next] = newCost;
					pq.Enqueue(next, newCost);
					cameFrom[next] = current;
				}
			}
		}

		return costSoFar.FirstOrDefault(kv => IsGoal(points, kv.Key)).Value;
	}

	private static IEnumerable<Point> Neighbours(
		Point p, int[][] points, Dictionary<Point, Point?> cameFrom)
	{
		if (p.X > 0 && LessThanThree((p1, p2) => p1.Y == p2.Y))
		{
			yield return new Point(p.X - 1, p.Y);
		}
		if (p.X < points.Length - 1 && LessThanThree((p1, p2) => p1.Y == p2.Y))
		{
			yield return new Point(p.X + 1, p.Y);
		}

		if (p.Y > 0 && LessThanThree((p1, p2) => p1.X == p2.X))
		{
			yield return new Point(p.X, p.Y - 1);
		}
		if (p.Y < points[0].Length - 1 && LessThanThree((p1, p2) => p1.X == p2.X))
		{
			yield return new Point(p.X, p.Y + 1);
		}

		bool LessThanThree(Func<Point, Point, bool> straightLineComparer)
		{
			if (cameFrom.TryGetValue(p, out var previous)
				&& previous.HasValue
				&& straightLineComparer(p, previous.Value)
				&& cameFrom.TryGetValue(previous.Value, out var previousPrevious)
				&& previousPrevious.HasValue
				&& straightLineComparer(p, previousPrevious.Value))
			{
				return false;
			}
			return true;
		}
	}
	private static bool IsGoal(int[][] points, Point p) =>
		p.X == points.Length - 1 && p.Y == points[0].Length - 1;

	record struct Point(int X, int Y);
}

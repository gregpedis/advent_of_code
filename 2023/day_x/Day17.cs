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
			if (IsGoal(points, current))
			{
				break;
			}

			foreach (var (next, cost) in Neighbours(current, points, cameFrom))
			{
				if (next == cameFrom[current])
				{
					continue;
				}

				var newCost = costSoFar[current] + cost;
				if (!costSoFar.TryGetValue(next, out var oldCost) || newCost < oldCost)
				{
					costSoFar[next] = newCost;
					pq.Enqueue(next, newCost);
					cameFrom[next] = current;
				}
			}
		}

		var res = costSoFar.FirstOrDefault(kv => IsGoal(points, kv.Key));

		var path = new List<Point>();
		Point? here = res.Key;
		while (here is not null)
		{
			path.Add(here.Value);
			here = cameFrom[here.Value];
		}

		for (int i = 0; i < points.Length; i++)
		{
			for (int j = 0; j < points[0].Length; j++)
			{
				if (path.Any(x => x.X == i && x.Y == j))
				{
					Console.Write(points[i][j]);
				}
				else
				{
					Console.Write('.');
				}
			}
			Console.WriteLine();
		}

		return res.Value;
	}

	private static IEnumerable<(Point, int)> Neighbours(
		Point p, int[][] points, Dictionary<Point, Point?> cameFrom)
	{
		var before = cameFrom[p];
		if (before is null || before.Value.Y != p.Y)
		{
			var cost = 0;
			foreach (var offset in Enumerable.Range(1, 3))
			{
				var next = new Point(p.X - offset, p.Y);
				if (InBounds(next))
				{
					cost += points[next.X][next.Y];
					yield return (next, cost);
				}
			}
			cost = 0;
			foreach (var offset in Enumerable.Range(1, 3))
			{
				var next = new Point(p.X + offset, p.Y);
				if (InBounds(next))
				{
					cost += points[next.X][next.Y];
					yield return (next, cost);
				}
			}
		}
		if (before is null || before.Value.X != p.X)
		{
			var cost = 0;
			foreach (var offset in Enumerable.Range(1, 3))
			{
				var next = new Point(p.X, p.Y - offset);
				if (InBounds(next))
				{
					cost += points[next.X][next.Y];
					yield return (next, cost);
				}
			}
			cost = 0;
			foreach (var offset in Enumerable.Range(1, 3))
			{
				var next = new Point(p.X, p.Y + offset);
				if (InBounds(next))
				{
					cost += points[next.X][next.Y];
					yield return (next, cost);
				}
			}
		}

		bool InBounds(Point p) =>
			p.X >= 0
			&& p.Y >= 0
			&& p.X < points.Length
			&& p.Y < points[0].Length;
	}
	private static bool IsGoal(int[][] points, Point p) =>
		p.X == points.Length - 1 && p.Y == points[0].Length - 1;

	record struct Point(int X, int Y);
}

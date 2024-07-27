namespace day_x;

internal static class Day17
{
	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day17.txt");

		Console.WriteLine("START");
		var part1 = PriorityBFS(ParseInput1(rows), Neighbours1);
		Console.WriteLine($"Total steps: {part1}");

		var part2 = PriorityBFS(ParseInput1(rows), Neighbours2);
		Console.WriteLine($"Total steps: {part2}");
	}

	private static int[][] ParseInput1(string[] rows)
	{
		var res = new List<int[]>();
		foreach (var row in rows)
		{
			res.Add([.. row.Select(x => x - '0')]);
		}
		return [.. res];
	}


	private static int PriorityBFS(
		int[][] points,
		Func<Point, Point?, int[][], IEnumerable<Point>> neighbours)
	{
		var pq = new PriorityQueue<(Point, Point?, int), int>();
		HashSet<(Point, Point?)> visited = new();

		var start = new Point(0, 0);
		pq.Enqueue((start, null, 0), 0);

		while (pq.Count > 0)
		{
			var (current, previous, cost) = pq.Dequeue();
			if (IsGoal(points, current))
			{
				return cost;
			}
			foreach (var next in neighbours(current, previous, points))
			{
				if (visited.Contains((next, current)))
				{
					continue;
				}
				var newCost = cost + TravelCost(current, next, points);
				pq.Enqueue((next, current, newCost), newCost);
				visited.Add((next, current));
			}
		}
		throw new NotImplementedException("wtf");
	}

	// Move 1 to 3 blocks
	private static IEnumerable<Point> Neighbours1(Point current, Point? previous, int[][] points) =>
		Neighbours(current, previous, points, 1, 3);

	// Move 4 to 10 blocks
	private static IEnumerable<Point> Neighbours2(Point current, Point? previous, int[][] points) =>
		Neighbours(current, previous, points, 4, 7);

	private static IEnumerable<Point> Neighbours(
		Point current, Point? previous, int[][] points, int minOffset, int count)
	{
		if (previous is null || previous.Value.X != current.X)
		{
			foreach (var offset in Enumerable.Range(minOffset, count))
			{
				var nextLeft = new Point(current.X, current.Y - offset);
				var nextRight = new Point(current.X, current.Y + offset);
				if (InBounds(nextLeft))
				{
					yield return nextLeft;
				}
				if (InBounds(nextRight))
				{
					yield return nextRight;
				}
			}
		}
		if (previous is null || previous.Value.Y != current.Y)
		{
			foreach (var offset in Enumerable.Range(minOffset, count))
			{
				var nextUp = new Point(current.X - offset, current.Y);
				var nextDown = new Point(current.X + offset, current.Y);
				if (InBounds(nextUp))
				{
					yield return nextUp;
				}
				if (InBounds(nextDown))
				{
					yield return nextDown;
				}
			}
		}

		bool InBounds(Point p) =>
			p.X >= 0
			&& p.Y >= 0
			&& p.X < points.Length
			&& p.Y < points[0].Length;
	}

	private static int TravelCost(Point from, Point to, int[][] points)
	{
		if (from.X == to.X)
		{
			var maxOffset = Math.Abs(from.Y - to.Y) - 1;
			var minY = Math.Min(from.Y, to.Y);
			return Enumerable.Range(1, maxOffset)
				.Select(offset => points[from.X][minY + offset])
				.Sum() + points[to.X][to.Y];
		}
		else if (from.Y == to.Y)
		{
			var maxOffset = Math.Abs(from.X - to.X) - 1;
			var minX = Math.Min(from.X, to.X);
			return Enumerable.Range(1, maxOffset)
				.Select(offset => points[minX + offset][from.Y])
				.Sum() + points[to.X][to.Y];
		}
		else
		{
			throw new NotImplementedException("wtf");
		}
	}

	private static bool IsGoal(int[][] points, Point p) =>
		p.X == points.Length - 1
		&& p.Y == points[0].Length - 1;

	record struct Point(int X, int Y);
}

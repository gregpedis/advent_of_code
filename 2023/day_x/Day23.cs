namespace day_x;

internal static class Day23
{
	private const char PATH = '.';
	private const char LEFT_SLOPE = '<';
	private const char RIGHT_SLOPE = '>';
	private const char DOWN_SLOPE = 'v';
	private const char UP_SLOPE = '^';

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day23.txt");
		Console.WriteLine(Part1(rows));
		Console.WriteLine(Part2(rows));
	}

	private static int Part1(string[] rows)
	{
		var start = FindStartingPoint(rows);
		var cameFrom = new Dictionary<Point, Point?>
		{
			{ start, null }
		};
		var costSoFar = new Dictionary<Point, int>
		{
			{ start, 0 }
		};
		var stack = new Stack<Point>();
		stack.Push(start);

		while (stack.TryPop(out var current))
		{
			var currentCost = costSoFar[current];
			foreach (var next in Neighbours(current, rows))
			{
				var (updatedNext, nextCost) = CalculateNextIfSlope(next, currentCost);
				if (SeenBefore(cameFrom, current, updatedNext))
				{
					continue;
				}
				else if (!costSoFar.TryGetValue(updatedNext, out var cost) || cost <= nextCost)
				{
					cameFrom[updatedNext] = current;
					costSoFar[updatedNext] = nextCost;
					stack.Push(updatedNext);
				}
			}
		}

		var res = costSoFar.First(x => x.Key.X == rows.Length - 1);
		PrettyPrint(rows, cameFrom, res.Key);
		return res.Value;

		static (Point Next, int Cost) CalculateNextIfSlope(Point point, int costSoFar)
		{
			return point.Type switch
			{
				Type.SlopeLeft => (new(point.X, point.Y - 1), costSoFar + 2),
				Type.SlopeRight => (new(point.X, point.Y + 1), costSoFar + 2),
				Type.SlopeDown => (new(point.X + 1, point.Y), costSoFar + 2),
				Type.SlopeUp => (new(point.X - 1, point.Y), costSoFar + 2),
				Type.Path => (point, costSoFar + 1),
				_ => throw new NotImplementedException()
			};
		}
	}

	private static int Part2(string[] rows)
	{
		var start = FindStartingPoint(rows);
		var end = FindEndingPoint(rows);
		var edges = Compress(MakeEdges(rows));

		var res = 0;
		Recurse(start, [start], 0);
		return res;

		void Recurse(Point current, HashSet<Point> visited, int costSoFar)
		{
			foreach (var edge in edges.Where(x => x.A == current))
			{
				var next = edge.B;
				if (!visited.Contains(next))
				{
					if (next == end)
					{
						res = Math.Max(res, costSoFar + edge.Cost);
					}
					else
					{
						Recurse(next, [next, .. visited], costSoFar + edge.Cost);
					}
				}
			}
		}
	}

	private static HashSet<Edge> MakeEdges(string[] rows)
	{
		var start = FindStartingPoint(rows);

		var edges = new HashSet<Edge>();
		var visited = new HashSet<Point>();

		var queue = new Queue<Point>();
		queue.Enqueue(start);
		while (queue.TryDequeue(out var current))
		{
			if (!visited.Contains(current))
			{
				var nexts = Neighbours(current, rows);
				foreach (var next in nexts)
				{
					queue.Enqueue(next);
					edges.Add(new(current, next, 1));
				}
				visited.Add(current);
			}
		}
		return edges;
	}

	private static HashSet<Edge> Compress(HashSet<Edge> edges)
	{
		while (FindRemovable(edges) is { } removable)
		{
			var alsoRemovable = edges.Where(x => x.B == removable.Key);
			foreach (var edge in removable)
			{
				edges.Remove(edge);
			}
			foreach (var edge in alsoRemovable)
			{
				edges.Remove(edge);
			}
			var first = removable.ElementAt(0);
			var second = removable.ElementAt(1);
			edges.Add(new(first.B, second.B, first.Cost + second.Cost));
			edges.Add(new(second.B, first.B, first.Cost + second.Cost));
		}
		return edges;

		static IGrouping<Point, Edge>? FindRemovable(HashSet<Edge> edges)
		{
			var grouped = edges.GroupBy(x => x.A).FirstOrDefault(x => x.Count() == 2);
			return grouped;
		}
	}

	private static bool SeenBefore(Dictionary<Point, Point?> cameFrom, Point current, Point next)
	{
		Point? previous = current;
		while (previous is not null)
		{
			if (previous == next)
			{
				return true;
			}
			else
			{
				previous = cameFrom[previous.Value];
			}
		}
		return false;
	}

	private static IEnumerable<Point> Neighbours(Point p, string[] rows)
	{
		if (p.X > 0 && GetType(rows, p.X - 1, p.Y) is { } t1)
		{
			yield return new(p.X - 1, p.Y, t1);
		}
		if (p.X < rows.Length - 1 && GetType(rows, p.X + 1, p.Y) is { } t2)
		{
			yield return new(p.X + 1, p.Y, t2);
		}
		if (p.Y > 0 && GetType(rows, p.X, p.Y - 1) is { } t3)
		{
			yield return new(p.X, p.Y - 1, t3);
		}
		if (p.Y < rows[p.X].Length - 1 && GetType(rows, p.X, p.Y + 1) is { } t4)
		{
			yield return new(p.X, p.Y + 1, t4);
		}
	}

	private static Type? GetType(string[] rows, int x, int y) =>
		rows[x][y] switch
		{
			PATH => Type.Path,
			LEFT_SLOPE => Type.SlopeLeft,
			RIGHT_SLOPE => Type.SlopeRight,
			UP_SLOPE => Type.SlopeUp,
			DOWN_SLOPE => Type.SlopeDown,
			_ => null
		};

	private static Point FindStartingPoint(string[] rows)
	{
		for (int i = 0; i < rows[0].Length; i++)
		{
			if (rows[0][i] == PATH)
			{
				return new(0, i);
			}
		}
		throw new NotImplementedException();
	}

	private static Point FindEndingPoint(string[] rows)
	{
		for (int i = 0; i < rows[^1].Length; i++)
		{
			if (rows[^1][i] == PATH)
			{
				return new(rows.Length - 1, i);
			}
		}
		throw new NotImplementedException();
	}

	private static void PrettyPrint(string[] rows, Dictionary<Point, Point?> cameFrom, Point lastPoint)
	{
		var route = new List<Point>([lastPoint]);
		var current = lastPoint;
		while (cameFrom.TryGetValue(current, out var previous) && previous is not null)
		{
			route.Add(previous.Value);
			current = previous.Value;
		}

		for (int i = 0; i < rows.Length; i++)
		{
			for (int j = 0; j < rows[i].Length; j++)
			{
				if (route.Exists(x => x.X == i && x.Y == j))
				{
					Console.Write('O');
				}
				else
				{
					Console.Write(rows[i][j]);
				}
			}
			Console.WriteLine();
		}
	}

	record struct Point(int X, int Y, Type Type = Type.Path);
	record struct Edge(Point A, Point B, int Cost)
	{
		public override readonly string ToString() =>
			$"({A.X},{A.Y}) - > ({B.X},{B.Y}) [{Cost}]";
	}

	enum Type
	{
		Path,
		SlopeUp,
		SlopeDown,
		SlopeLeft,
		SlopeRight,
	}
}

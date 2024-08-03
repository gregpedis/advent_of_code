using System.Numerics;

namespace day_x;

internal static class Day21
{
	private const char STARTING = 'S';
	private const char INVALID = '#';
	private const int TOTAL_STEPS = 64;
	private const int TOTAL_STEPS2 = 26501365;

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day21.txt");
		Console.WriteLine(Part1(rows));
		Console.WriteLine(Part2(rows));
	}

	private static int Part1(string[] rows)
	{
		return FloodFill(rows, TOTAL_STEPS, Neighbours1, IsValid);

		static bool IsValid(Point p, string[] rows) =>
			rows[p.X][p.Y] != INVALID;
	}

	// https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/kebnr7e/
	// https://en.wikipedia.org/wiki/Lagrange_polynomial
	private static BigInteger Part2(string[] rows)
	{
		// [65, 196, 327]
		var x1 = rows.Length / 2;
		var x2 = x1 + rows.Length;
		var x3 = x2 + rows.Length;

		// [3944, 35082, 97230]
		BigInteger fx1 = FloodFill(rows, x1, (p, _, _) => Neighbours2(p), IsValid);
		BigInteger fx2 = FloodFill(rows, x2, (p, _, _) => Neighbours2(p), IsValid);
		BigInteger fx3 = FloodFill(rows, x3, (p, _, _) => Neighbours2(p), IsValid);

		var arg1 = fx1 * (TOTAL_STEPS2 - x2) / (x1 - x2) * (TOTAL_STEPS2 - x3) / (x1 - x3);
		var arg2 = fx2 * (TOTAL_STEPS2 - x1) / (x2 - x1) * (TOTAL_STEPS2 - x3) / (x2 - x3);
		var arg3 = fx3 * (TOTAL_STEPS2 - x1) / (x3 - x1) * (TOTAL_STEPS2 - x2) / (x3 - x2);
		var res = arg1 + arg2 + arg3;

		return res;

		static bool IsValid(Point p, string[] rows)
		{
			var modX = p.X % rows.Length;
			var modY = p.Y % rows[0].Length;

			var newX = modX < 0
				? rows.Length + modX
				: modX;

			var newY = modY < 0
				? rows[0].Length + modY
				: modY;

			return rows[newX][newY] != INVALID;
		}
	}

	private static int FloodFill(
		string[] rows,
		int totalSteps,
		Func<Point, int, int, IEnumerable<Point>> neighbours,
		Func<Point, string[], bool> isValid
		)
	{
		var visited = new HashSet<Point>();
		var valid = new HashSet<Point>();
		List<Point> frontier = [FindStartingLocation(rows)];

		for (int i = 1; i <= totalSteps; i++)
		{
			frontier = TakeStep(frontier, visited, rows, neighbours, isValid);
			if (i % 2 == totalSteps % 2)
			{
				foreach (var point in frontier)
				{
					valid.Add(point);
				}
			}
		}

		return valid.Count;
	}

	// finite grid
	private static List<Point> TakeStep(
		List<Point> frontier,
		HashSet<Point> visited,
		string[] rows,
		Func<Point, int, int, IEnumerable<Point>> neighbours,
		Func<Point, string[], bool> isValid
		)
	{
		var nextFrontier = new List<Point>();

		foreach (var current in frontier)
		{
			foreach (var next in neighbours(current, rows.Length, rows[0].Length))
			{
				if (visited.Contains(next))
				{
					continue;
				}

				if (isValid(next, rows))
				{
					visited.Add(next);
					nextFrontier.Add(next);
				}
			}
		}
		return nextFrontier;
	}

	private static IEnumerable<Point> Neighbours1(Point p, int maxX, int maxY)
	{
		if (p.X > 0)
		{
			yield return new(p.X - 1, p.Y);
		}
		if (p.X < maxX - 1)
		{
			yield return new(p.X + 1, p.Y);
		}
		if (p.Y > 0)
		{
			yield return new(p.X, p.Y - 1);
		}
		if (p.Y < maxY - 1)
		{
			yield return new(p.X, p.Y + 1);
		}
	}

	private static IEnumerable<Point> Neighbours2(Point p)
	{
		yield return new(p.X - 1, p.Y);
		yield return new(p.X + 1, p.Y);
		yield return new(p.X, p.Y - 1);
		yield return new(p.X, p.Y + 1);
	}

	private static Point FindStartingLocation(string[] rows)
	{
		for (int i = 0; i < rows.Length; i++)
		{
			for (int j = 0; j < rows[i].Length; j++)
			{
				if (rows[i][j] == STARTING)
				{
					return new(i, j);
				}
			}
		}
		throw new NotImplementedException();
	}

	record struct Point(int X, int Y);
}

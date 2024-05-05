using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;

namespace day_x;

internal static class Day11
{
	const char GALAXY = '#';
	const char EMPTY = '.';
	const int EXPANSION = 1_000_000;

	public static void Solve()
	{
		var space = InputReader.ReadRows("Day11.txt");

		Part1(space);
		Part2(space);
	}

	static void Part1(string[] space)
	{
		var fixedSpace = FixSpace(space);
		var galaxies = GetGalaxies(fixedSpace);
		var distances = GetManhattanDistances(galaxies);

		var distanceSum = distances.Sum();

		Console.WriteLine($"Distance sum: {distanceSum}");
	}

	static void Part2(string[] space)
	{
		var galaxies = GetGalaxies(space);
		var distances = new List<BigInteger>();

		for (int i = 0; i < galaxies.Count; i++)
		{
			for (int j = i + 1; j < galaxies.Count; j++)
			{
				var d = galaxies[i].DistanceTo(galaxies[j], space);
				distances.Add(d);
			}
		}

		BigInteger distanceSum = 0;

		foreach (var item in distances)
		{
			distanceSum += item;
		}

		Console.WriteLine($"Distance sum: {distanceSum}");
	}

	static string[] FixSpace(string[] space)
	{
		space = Expand(space);
		space = Transpose(space);
		space = Expand(space);
		space = Transpose(space);
		return space;
	}

	static List<Point> GetGalaxies(string[] rows)
	{
		var res = new List<Point>();

		for (int i = 0; i < rows.Length; i++)
		{
			for (int j = 0; j < rows[0].Length; j++)
			{
				if (rows[i][j] == GALAXY)
				{
					res.Add(new(i, j));
				}
			}
		}
		return res;
	}

	static List<int> GetManhattanDistances(List<Point> points)
	{
		var distances = new List<int>();

		for (int i = 0; i < points.Count; i++)
		{
			for (int j = i + 1; j < points.Count; j++)
			{
				var d = points[i].DistanceTo(points[j]);
				distances.Add(d);
			}
		}

		return distances;
	}

	static string[] Transpose(string[] rows)
	{
		var transposed = new List<string>();

		for (int j = 0; j < rows[0].Length; j++)
		{
			var row = new StringBuilder();
			for (int i = 0; i < rows.Length; i++)
			{
				row.Append(rows[i][j]);
			}

			transposed.Add(row.ToString());
		}
		return [.. transposed];
	}

	static string[] Expand(string[] space)
	{
		var res = new List<string>();

		foreach (var row in space)
		{
			res.Add(row);
			if (row.All(x => x == EMPTY))
			{
				res.Add(row);
			}
		}
		return [.. res];
	}

	record Point(int x, int y)
	{
		public int DistanceTo(Point other) =>
			int.Abs(x - other.x) + int.Abs(y - other.y);

		public BigInteger DistanceTo(Point other, string[] space)
		{
			BigInteger res = Math.Abs(x - other.x) + Math.Abs(y - other.y);

			var min_x = Math.Min(x, other.x);
			var max_x = Math.Max(x, other.x);
			for (int i = min_x + 1; i < max_x; i++)
			{
				if (space[i].All(x => x == EMPTY))
				{
					res += EXPANSION - 1;
				}
			}

			var min_y = Math.Min(y, other.y);
			var max_y = Math.Max(y, other.y);
			for (int j = min_y + 1; j < max_y; j++)
			{
				if (space.Select(row => row[j]).All(x => x == EMPTY))
				{
					res += EXPANSION - 1;
				}
			}

			return res;
		}
	}
}

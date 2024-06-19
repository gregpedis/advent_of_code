using System.Numerics;

namespace day_x;

internal static class Day24
{
	 static double MIN_DIMENSION = 200000000000000;
	 static double MAX_DIMENSION = 400000000000000;


	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day24.txt");

		var part1 = Solve1(ParseInput1(rows));
		Console.WriteLine($"Collisions: {part1}");
	}


	static int Solve1(List<Instruction> instructions)
	{
		var total = 0;
		var lines = instructions.Select(CreateLine).ToList();

		for (int i = 0; i < lines.Count; i++)
		{
			for (int j = i + 1; j < lines.Count; j++)
			{
				var l1 = lines[i];
				var l2 = lines[j];

				var p = Colision(l1, l2);

				if (p is { } point
					&& InsideArea(point)
					&& InFuture(l1, point)
					&& InFuture(l2, point))
				{
					total++;
				}
			}
		}

		return total;
	}

	static Point? Colision(Line l1, Line l2)
	{
		if (l1.Slope == l2.Slope)
		{
			return null;
		}

		var x = (l2.Offset - l1.Offset) / (l1.Slope - l2.Slope);
		var y = l1.Slope * x + l1.Offset;
		Console.WriteLine($"Collision at: {x} +++ {y}");
		return new(x, y, 0);
	}

	static bool InFuture(Line l, Point p) =>
		Distance(l.A, p) > Distance(l.B, p);

	static double Distance(Point A, Point B) =>
		double.Pow(A.X - B.X, 2) + double.Pow(A.Y - B.Y, 2);

	static bool InsideArea(Point p) =>
		p.X <= MAX_DIMENSION
		&& p.X >= MIN_DIMENSION
		&& p.Y <= MAX_DIMENSION
		&& p.Y >= MIN_DIMENSION;

	static Line CreateLine(Instruction i)
	{
		var point2 = new Point(
			i.P.X + i.Vx,
			i.P.Y + i.Vy,
			0
			);

		var slope = (i.P.Y - point2.Y) / (i.P.X - point2.X);
		var offset = i.P.Y - slope * i.P.X;
		return new Line(i.P, point2, slope, offset);
	}

	static List<Instruction> ParseInput1(string[] instructions)
	{
		var res = new List<Instruction>();

		foreach (var data in instructions)
		{
			var splitted = data.Split('@');
			var positions = splitted[0].Split(',');
			var speeds = splitted[1].Split(',');

			res.Add(new(
				new(
					double.Parse(positions[0]),
					double.Parse(positions[1]),
					0
				),
				int.Parse(speeds[0]),
				int.Parse(speeds[1]),
				0
				));
		}

		return res;
	}

	record struct Instruction(Point P, int Vx, int Vy, int Vz);

	record struct Point(double X, double Y, double Z);

	// y = Slope * x + Offset
	record struct Line(Point A, Point B, double Slope, double Offset);
}


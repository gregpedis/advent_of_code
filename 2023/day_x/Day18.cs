using System.Linq;


namespace day_x;

internal static class Day18
{
	static Dictionary<Direction, Func<Point, string, Point>> NextSteps = new()
	{
		{  Direction.Up, (p, rgb) => new (p.X-1, p.Y, rgb)},
		{  Direction.Down, (p, rgb) => new (p.X+1, p.Y, rgb)},
		{  Direction.Left, (p, rgb) => new (p.X, p.Y-1, rgb)},
		{  Direction.Right, (p, rgb) => new (p.X, p.Y+1, rgb)},
	};

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day18.txt");
		var instructions = ParseInput(rows);

		var res = Part1(instructions);
		Console.WriteLine($"Enclosed points count: {res}");
	}

	static int Part1(List<Instruction> instructions)
	{
		var grid = CreateGrid(instructions);
		PrintGrid(grid);

		Console.WriteLine();
		FixInside(grid);
		PrintGrid(grid);

		return -1;
	}

	static void FixInside(Point[][] grid)
	{
		for (int i = 0; i < grid.Length; i++)
		{
			for (int j = 0; j < grid[0].Length; j++)
			{
				if (grid[i][j] is null && IsInside(grid, i, j))
				{
					grid[i][j] = new(i, j, string.Empty, true);
				}
			}
		}
	}

	// TODO: Maybe do it in every direction?
	// TODO: Flood fill? Either from a guaranteed inside point, or just flood fill the outside.
	static bool IsInside(Point[][] grid, int x, int y)
	{
		// start as inside
		var intersections = 0;
		// need to know if we hit a continuous line
		var previousTrench = false;

		var steps = x;
		for (int i = 1; i <= steps; i++)
		{
			x -= 1;
			if (grid[x][y] is { } trench && !trench.Inside)
			{
				if (!previousTrench)
				{
					intersections++;
					previousTrench = true;
				}
			}
			else
			{
				previousTrench = false;
			}
		}

		return intersections % 2 == 1;
	}

	static Point[][] CreateGrid(List<Instruction> instructions)
	{
		var points = new List<Point>();

		var currentPoint = new Point(0, 0, string.Empty);
		foreach (var instr in instructions)
		{
			var callback = NextSteps[instr.Heading];

			for (int i = 0; i < instr.Steps; i++)
			{
				currentPoint = callback(currentPoint, instr.RGB);
				points.Add(currentPoint);
			}
		}

		var grid = ToGrid(points);

		return grid;
	}

	static void PrintGrid(Point[][] grid)
	{
		for (int i = 0; i < grid.Length; i++)
		{
			for (int j = 0; j < grid[0].Length; j++)
			{
				var v = grid[i][j] is null ? '.' : '#';
				Console.Write(v);
			}
			Console.WriteLine();
		}
	}

	static Point[][] ToGrid(List<Point> points)
	{
		var minX = points.Select(p => p.X).Min();
		var minY = points.Select(p => p.Y).Min();

		var normalized = points
			.Select(p => new Point(p.X - minX, p.Y - minY, p.RGB))
			.ToList();

		var grid = new List<Point[]>();
		for (int i = 0; i <= normalized.MaxBy(p => p.X).X; i++)
		{
			var row = new List<Point>();
			for (int j = 0; j <= normalized.MaxBy(p => p.Y).Y; j++)
			{
				if (normalized.FirstOrDefault(p => p.X == i && p.Y == j) is { } p)
				{
					row.Add(p);
				}
				else
				{
					row.Add(null);
				}

			}
			grid.Add([.. row]);
		}

		return [.. grid];
	}

	static List<Instruction> ParseInput(string[] instructions)
	{
		var res = new List<Instruction>();

		foreach (var data in instructions)
		{
			var splitted = data.Split(' ');
			var direction = splitted[0][0] switch
			{
				'U' => Direction.Up,
				'D' => Direction.Down,
				'L' => Direction.Left,
				'R' => Direction.Right,
				_ => throw new NotImplementedException("wtf"),
			};
			var steps = int.Parse(splitted[1]);
			var rgb = splitted[2][1..^1];

			res.Add(new(direction, steps, rgb));
		}

		return res;
	}


	record class Point(int X, int Y, string RGB, bool Inside = false);

	record struct Instruction(Direction Heading, int Steps, string RGB);

	enum Direction
	{
		Up,
		Down,
		Left,
		Right
	}
}


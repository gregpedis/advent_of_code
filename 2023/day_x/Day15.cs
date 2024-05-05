using System.Linq;


namespace day_x;

internal static class Day15
{
	public static void Solve()
	{
		var grid = InputReader.ReadRows("Day15.txt");

		var mainLoop = new List<Point>();

		var startingPoint = FindStart(grid);
		mainLoop.Add(new(startingPoint));

		var previous = startingPoint;
		var current = FindNextPoint(grid, startingPoint, previous);

		while (current != startingPoint)
		{
			mainLoop.Add(new(current));
			var temp = current;
			current = FindNextPoint(grid, current, previous);
			previous = temp;
		}


		Console.WriteLine(startingPoint);
		Console.WriteLine($"Main Loop length: {mainLoop.Count}");
		Console.WriteLine($"Maximum steps: {mainLoop.Count / 2}");

		var points = FindEnclosedPoints(mainLoop);
		Console.WriteLine($"Enclosed points count: {points}");
	}

	private static int FindEnclosedPoints(List<Point> loop)
	{
		var area = 0;
		// Shoelace
		for (int i = 0; i < loop.Count; i++)
		{
			// current point
			var p1 = loop[i];
			// next point, or first point if we are at last point
			var p2 = i == loop.Count - 1 ? loop[0] : loop[i + 1];
			// Fucking Gauss
			area += p1.x * p2.y - p1.y * p2.x;
		}
		// Ignore order of calculating the polygon
		area = Math.Abs(area) / 2;
		// Pick's theorem reversed
		return area - loop.Count / 2 + 1;
	}

	private static LoopPoint FindStart(string[] grid)
	{
		for (int i = 0; i < grid.Length; i++)
		{
			for (int j = 0; j < grid[0].Length; j++)
			{
				if (grid[i][j] == 'S')
				{
					return new(i, j, PipeType.StartingPosition);
				}
			}

		}
		throw new Exception("what the fuck is this input");
	}

	private static LoopPoint FindNextPoint(string[] grid, LoopPoint current, LoopPoint previous)
	{
		if (CanMoveUp(current.Row, current.Column, grid))
		{
			var type = GetPipeType(grid[current.Row - 1][current.Column]);
			var nextPoint = new LoopPoint(current.Row - 1, current.Column, type);
			if (nextPoint != previous)
			{
				return nextPoint;
			}
		}
		if (CanMoveDown(current.Row, current.Column, grid))
		{
			var type = GetPipeType(grid[current.Row + 1][current.Column]);
			var nextPoint = new LoopPoint(current.Row + 1, current.Column, type);
			if (nextPoint != previous)
			{
				return nextPoint;
			}
		}
		if (CanMoveRight(current.Row, current.Column, grid))
		{
			var type = GetPipeType(grid[current.Row][current.Column + 1]);
			var nextPoint = new LoopPoint(current.Row, current.Column + 1, type);
			if (nextPoint != previous)
			{
				return nextPoint;
			}
		}
		if (CanMoveLeft(current.Row, current.Column, grid))
		{
			var type = GetPipeType(grid[current.Row][current.Column - 1]);
			var nextPoint = new LoopPoint(current.Row, current.Column - 1, type);
			if (nextPoint != previous)
			{
				return nextPoint;
			}
		}

		throw new Exception("what the fuck is this input");
	}

	#region MOVEMENT

	private static bool CanMoveLeft(int i, int j, string[] grid)
	{
		if (j == 0) return false;

		var pipeFrom = GetPipeType(grid[i][j]);
		var pipeTo = GetPipeType(grid[i][j - 1]);

		return CanMove(
			pipeFrom,
			pipeTo,
			[PipeType.UpLeft, PipeType.DownLeft, PipeType.Vertical],
			[PipeType.UpLeft, PipeType.DownLeft, PipeType.Horizontal]
			);
	}

	private static bool CanMoveRight(int i, int j, string[] grid)
	{
		if (j == grid[i].Length - 1) return false;

		var pipeFrom = GetPipeType(grid[i][j]);
		var pipeTo = GetPipeType(grid[i][j + 1]);

		return CanMove(
			pipeFrom,
			pipeTo,
			[PipeType.UpRight, PipeType.DownRight, PipeType.Vertical],
			[PipeType.UpRight, PipeType.DownRight, PipeType.Horizontal]
			);
	}

	private static bool CanMoveDown(int i, int j, string[] grid)
	{
		if (i == grid.Length - 1) return false;

		var pipeFrom = GetPipeType(grid[i][j]);
		var pipeTo = GetPipeType(grid[i + 1][j]);

		return CanMove(
			pipeFrom,
			pipeTo,
			[PipeType.DownLeft, PipeType.DownRight, PipeType.Horizontal],
			[PipeType.DownLeft, PipeType.DownRight, PipeType.Vertical]
			);
	}

	private static bool CanMoveUp(int i, int j, string[] grid)
	{
		if (i == 0) return false;

		var pipeFrom = GetPipeType(grid[i][j]);
		var pipeTo = GetPipeType(grid[i - 1][j]);

		return CanMove(
			pipeFrom,
			pipeTo,
			[PipeType.UpLeft, PipeType.UpRight, PipeType.Horizontal],
			[PipeType.UpLeft, PipeType.UpRight, PipeType.Vertical]
			);
	}

	private static bool CanMove(
		PipeType pipeFrom,
		PipeType pipeTo,
		PipeType[] illegalFrom,
		PipeType[] legalTo)
	{
		return !illegalFrom.Contains(pipeFrom)
			&& (legalTo.Contains(pipeTo) || pipeTo == PipeType.StartingPosition);
	}

	#endregion

	#region TYPE_HELPING

	private static PipeType GetPipeType(char c) =>
		c switch
		{
			'S' => PipeType.StartingPosition,
			'-' => PipeType.Horizontal,
			'|' => PipeType.Vertical,
			'F' => PipeType.UpLeft,
			'J' => PipeType.DownRight,
			'7' => PipeType.UpRight,
			'L' => PipeType.DownLeft,
			'.' => PipeType.NOT_A_PIPE,
			'O' => PipeType.NOT_A_PIPE,
			'I' => PipeType.NOT_A_PIPE,
			_ => throw new Exception("what the fuck is this input")
		};

	enum PipeType
	{
		StartingPosition,
		NOT_A_PIPE,
		UpLeft,
		UpRight,
		DownLeft,
		DownRight,
		Vertical,
		Horizontal,
	}

	record struct LoopPoint(int Row, int Column, PipeType Type);

	record struct Point
	{
		public int x;
		public int y;

		public Point(LoopPoint point)
		{
			this.x = point.Row;
			this.y = point.Column;
		}
	}
	#endregion
}


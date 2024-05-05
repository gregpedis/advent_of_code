namespace day_x;

internal static class Day16
{
	const char PASSTHROUGH = '.';
	const char MIRROR_RIGHT = '/';
	const char MIRROR_LEFT = '\\';
	const char VERTICAL = '|';
	const char HORIZONTAL = '-';

	public static void Solve()
	{
		var grid = InputReader.ReadRows("Day16.txt");

		var res = Part1(grid);
		Console.WriteLine($"Total energized tiles: {res}");

		var res2 = Part2(grid);
		Console.WriteLine($"Total energized tiles ANY starting position: {res2}");
	}


	static int Part2(string[] grid)
	{
		var results = new List<int>();

		for (int i = 0; i < grid.Length; i++)
		{
			var res1= Solve(grid, new(i, -1, Direction.Right));
			var res2 = Solve(grid, new(i, grid[0].Length, Direction.Left));
			results.Add(res1);
			results.Add(res2);
		}

		for (int i = 0; i < grid[0].Length; i++)
		{
			var res1 = Solve(grid, new(-1, i, Direction.Down));
			var res2 = Solve(grid, new(grid.Length, i, Direction.Up));
			results.Add(res1);
			results.Add(res2);
		}

		return results.Max();
	}

	static int Part1(string[] grid) =>
		Solve(grid, new(0, -1, Direction.Right));

	static int Solve(string[] grid, Point initial)
	{
		var visited = new HashSet<Point>();

		var toProcess = new Stack<Point>();
		toProcess.Push(initial);

		while (toProcess.Count > 0)
		{
			var current = toProcess.Pop();
			if (visited.Contains(current))
			{
				continue;
			}

			visited.Add(current);

			foreach (var state in NextStates(grid, current))
			{
				toProcess.Push(state);
			}
		}

		//PrintGrid(grid, visited);
		return visited.Select(p => (p.X, p.Y)).Distinct().Count() - 1;
	}

	static void PrintGrid(string[] grid, HashSet<Point> visited)
	{
		for (int i = 0; i < grid.Length; i++)
		{
			for (int j = 0; j < grid[0].Length; j++)
			{
				if (visited.Any(p => p.X == i && p.Y == j))
				{
					Console.Write("#");
				}
				else
				{
					Console.Write(".");
				}
			}

			Console.WriteLine();
		}
	}

	static IEnumerable<Point> NextStates(string[] grid, Point current)
	{
		var (nextX, nextY) = NextPosition(current);

		if (!InBounds(nextX, nextY, grid))
		{
			yield break;
		}

		foreach (var direction in NextDirections(current.Heading, grid[nextX][nextY]))
		{
			yield return new(nextX, nextY, direction);
		}
	}

	static (int X, int Y) NextPosition(Point current) =>
		current.Heading switch
		{
			Direction.Up => (current.X - 1, current.Y),
			Direction.Down => (current.X + 1, current.Y),
			Direction.Left => (current.X, current.Y - 1),
			Direction.Right => (current.X, current.Y + 1),
			_ => throw new NotImplementedException("WTF"),
		};

	static Direction[] NextDirections(Direction incoming, char next) =>
		(incoming, next) switch
		{
			// Keep going
			(_, PASSTHROUGH) => [incoming],
			(Direction.Right, HORIZONTAL) => [incoming],
			(Direction.Left, HORIZONTAL) => [incoming],
			(Direction.Up, VERTICAL) => [incoming],
			(Direction.Down, VERTICAL) => [incoming],
			// Split
			(Direction.Right, VERTICAL) => [Direction.Up, Direction.Down],
			(Direction.Left, VERTICAL) => [Direction.Up, Direction.Down],
			(Direction.Up, HORIZONTAL) => [Direction.Right, Direction.Left],
			(Direction.Down, HORIZONTAL) => [Direction.Right, Direction.Left],
			// Redirect by \
			(Direction.Right, MIRROR_LEFT) => [Direction.Down],
			(Direction.Left, MIRROR_LEFT) => [Direction.Up],
			(Direction.Up, MIRROR_LEFT) => [Direction.Left],
			(Direction.Down, MIRROR_LEFT) => [Direction.Right],
			// Redirect by /
			(Direction.Right, MIRROR_RIGHT) => [Direction.Up],
			(Direction.Left, MIRROR_RIGHT) => [Direction.Down],
			(Direction.Up, MIRROR_RIGHT) => [Direction.Right],
			(Direction.Down, MIRROR_RIGHT) => [Direction.Left],
			_ => throw new NotImplementedException("WTF2"),
		};

	static bool InBounds(int x, int y, string[] grid) =>
			x >= 0
			&& y >= 0
			&& x < grid.Length
			&& y < grid[0].Length;

	record struct Point(int X, int Y, Direction Heading);

	enum Direction
	{
		Up,
		Down,
		Left,
		Right
	}
}


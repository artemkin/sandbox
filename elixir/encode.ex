defmodule Stat do

  @doc ~S"""
  Aggregates consecutive duplicates into {value, count} tuples

  ## Examples

      iex> Stat.aggregate []
      []

      iex> Stat.aggregate [:hello]
      [{:hello, 1}]

      iex> Stat.aggregate ["foo", "foo", "bar", "baz", "baz", "baz", "baz"]
      [{"foo", 2}, {"bar", 1}, {"baz", 4}]

  """
  def aggregate([]), do: []
  def aggregate([elem]), do: [{elem, 1}]
  def aggregate(lst) do
    {acc, last, count} = List.foldl tl(lst), {[], hd(lst), 1}, fn (x, {acc, prev, count}) ->
      if x != prev do
        {[{prev, count} | acc], x, 1}
      else
        {acc, x, count + 1}
      end
    end
    [{last, count} | acc]
    |> Enum.reverse
  end

  @doc ~S"""
  Encodes consecutive duplicates into unique values.

  ## Examples

      iex> Stat.encode_duplicates [], 1
      []

      iex> Stat.encode_duplicates [1, 2, 3], -1
      []

      iex> Stat.encode_duplicates [1, 2, 2, 2, 3, 3], 0
      [1, 2, 3]

      iex> Stat.encode_duplicates [1, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 5], 1
      [10, 20, 30, 31, 32, 40, 41, 50, 51, 52, 53, 54]

      iex> Stat.encode_duplicates [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3], 1
      [10, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]

      iex> Stat.encode_duplicates [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3], 1
      [10, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]

      iex> Stat.encode_duplicates [1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3], 2
      [100, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 300]

  """
  def encode_duplicates([], _), do: []
  def encode_duplicates(_, factor) when factor < 0, do: []
  def encode_duplicates(lst, factor) do
    factor = :math.pow(10, factor) |> round
    encode = fn {value, count} ->
      0..(count - 1) |> Enum.map(&(&1 + value * factor))
    end
    lst
    |> aggregate
    |> Enum.map(fn {value, count} -> {value, min(count, factor)} end)
    |> Enum.map(encode)
    |> Enum.concat
  end

end


// Mac OS
// brew install google-benchmark
// c++ -std=c++14 -O3 -I/usr/local/include -L/usr/local/lib -lbenchmark ring_buf_bench.cpp

#include <benchmark/benchmark.h>
#include <iostream>

constexpr size_t MaxSize = 8192; // power of 2

static void MB_Vector(benchmark::State& state)
{
    std::vector<int> v;
    v.reserve(MaxSize);
    for (size_t i = 0; i < MaxSize; ++i)
        v.push_back(i);

    int result = 0;
    while (state.KeepRunning())
    {
        for (size_t i = 0; i < MaxSize; ++i)
            result += v[i];
    }

    benchmark::DoNotOptimize(result);
}

static void MB_RingBufferMod(benchmark::State& state)
{
    std::vector<int> v;
    v.reserve(MaxSize);
    for (size_t i = 0; i < MaxSize; ++i)
        v.push_back(i);

    const size_t beginIndex = MaxSize / 2;

    int result = 0;
    while (state.KeepRunning())
    {
        for (size_t i = 0; i < MaxSize; ++i)
        {
            const auto index = (beginIndex + i) % MaxSize;
            result += v[index];
        }
    }

    benchmark::DoNotOptimize(result);
}

static void MB_RingBufferIf(benchmark::State& state)
{
    std::vector<int> v;
    v.reserve(MaxSize);
    for (size_t i = 0; i < MaxSize; ++i)
        v.push_back(i);

    const size_t beginIndex = MaxSize / 2;

    int result = 0;
    while (state.KeepRunning())
    {
        for (size_t i = 0; i < MaxSize; ++i)
        {
            auto index = (beginIndex + i);
            if (index >= v.size())
                index -= MaxSize;
            result += v[index];
        }
    }

    benchmark::DoNotOptimize(result);
}

BENCHMARK(MB_Vector);
BENCHMARK(MB_RingBufferMod);
BENCHMARK(MB_RingBufferIf);

BENCHMARK_MAIN();

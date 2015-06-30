
// Closures are reference types in Swift.
// They automatically capture variables by reference if they are mutated

func generate() -> [() -> Int] {
    var result:[() -> Int] = []

    var counter = 0

    for _ in 1...10 {
        func increment() -> Int {
            return counter++;
        }
        result.append(increment)
    }

    return result;
}

let seq = generate()
let seq2 = seq

for s in seq {
    print(s())
}

for s in seq2 {
    print(s())
}


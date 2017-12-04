
# Weather challenge

A few notes:

 - Wow, this was a much meatier challenge than option 1 -- there's WAY more to be done here.
 - I'm not as experienced in Haskell as I am in other languages, so I took extra time to work my way through this one. For example, this is my first Stack project.
 - There are quite a few places where my coding style could be considered messy, non-idiomatic, or inconsistent with itself. I'm still learning how to write good Haskell code.

## How to run

As I understand, you should be able to just clone it and `stack test` to get started.

```stack exec weather-exe generate filename.txt 50000```

This will generate 50 thousand lines of sample test data, and save it in `filename.txt` in the current directory.
Any unrecognized/malformed lines will be written to `errors.log`, in the current directory.

```stack exec weather-exe parse filename.txt```

This will read all lines from `filename.txt`, and display statistics about the flight.
 
```stack test```

This will run the HSpec and QuickCheck tests. (There's only a few, but the QuickCheck test covers a lot of possible failure cases.)

## Requirements coverage

### Data format

> The location is a co-ordinate x,y. And x, and y are natural numbers in observatory specific units.

This is weird, considering that the balloon is travelling worldwide. There's no way that I'm aware that an x,y position in the world can be expressed in meters, unless the world is a torus or flat polygon.

For now, I'm going to assume that each flight of the balloon travels within a smallish bounded grid, and because of that, we can assume the world is flat. But this would need to be addressed in the first code review (if this was real code).

> Data from the balloon is of varying quality, so don't make any assumptions about the quality of the input.

To handle this, I added some code so that any lines that fail to be parsed will be saved in `errors.log`. This way, if the observatories are sending shoddy data, we'd be able to refine our parser over time.

> Data from the balloon often comes in large batches, so assume you may need to deal with data that doesn't fit in memory.

I really haven't addressed this line in the requirements. The code is written with the assumption that the entire file can be parsed into memory.

If I were to address this, I would most likely feed the data into a database, and run all operations against the database. (Or, possibly index the file on-disk with Apache Lucene.)

### Tasks

> 1. Given that it is difficult to obtain real data from the weather balloon we would first like to be able to generate a test file of representative (at least in form) data for use in simulation and testing. This tool should be able to generate at least 500 million lines of data for testing your tool. Remember that the data is not reliable, so consider including invalid and out of order lines.

This is working pretty well.

Some areas to improve:

 - `time stack exec weather-exe generate filename.txt 500000` shows `real   0m19.891s`. I haven't tried generating 500 million lines. If we assume linear scaling then it's going to take at least 13 days to generate this on my laptop. We may need to optimize the generator, or run it on some AWS hardware.
 - The "bad data" generation isn't super sophisticated. We could create more realistic bad data.
 - More tests could be useful

Overall, the generation functionality seems to be working pretty well.

> 2. Produce statistics of the flight. The program should be able to compute any combination of the following on request:

As far as I can see, this is working.

>     The minimum temperature.
>     The maximum temperature.
>     The mean temperature.

I believe these are being computed in O(n) time and O(1) memory.

>     The number of observations from each observatory.

I used a Data.Map for this, so my estimate of the complexity class is O(n log k) time and O(k log k) space, where k is the number of distinct observatories.
If we used a hashtable instead then it'd drop to O(n) time and O(k) space.

>     The total distance travelled.

My algorithm is using O(n log n) time and space (because the data gets sorted by timestamp before proceeding.) This sort would cause us problems if the data set is too large to fit in memory. I'd most likely offload the data to a database to fix this. But I've pushed it out of scope for this exercise due to time constraints :)

Some areas to improve:
 - more tests! (this is the part that would benefit the most from extra tests)
 - ability to generate statistics in other temperature/distance units
 - As noted in the requirements, we should be able to generate statistics even when the data set is too large to fit into memory. (I don't see a way to do this for "total distance travelled" unless the data is stored in a database, or we somehow sort the entries on-disk so that they can be parsed in timestamp order.)

> 3. Produce a normalized output of the data, where given desired units for temperature and distance, an output file is produced containing all observations with the specified output units.

The conversion functions exist in the code, but there is no ability to generate an output file with these units.

At this point, it would be pretty trivial to add -- it would just be a couple of additional command-line params for the `generate` command.

package ru.ifmo.onell.problem;

public class WModelRunner {
    native double runWModel(int dimension, double dummy, int epistasis,
    int neutrality, int ruggedness, int[] ind);

    native double getOptimum(int dimension, double dummy, int epistasis,
    int neutrality, int ruggedness);

    static {
        System.loadLibrary("wmodel");
     }
}

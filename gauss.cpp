#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>

using namespace std;

template<typename T>
class Matrix {
private:
    vector<vector<T>> data;
public:
    Matrix(vector<vector<T>> data) : data{data}  { }

    Matrix(size_t size){
        vector<vector<double>> temp(size, vector<double>(size));
        data = temp;
    }

    Matrix(Matrix first, Matrix second) {
        for (size_t i = 0; i < first.size(); ++i) {
            first(i).push_back(second(i, 0));
        }
        data = first();
    }

    Matrix() : Matrix{0} { }

    void operator=(const vector<vector<T>>& newData) {
        data = newData;
    }

    void swap(int i, int k) {
        for (size_t j = 0; j <= data.size(); ++j) {
            double temp = data[i][j];
            data[i][j] = data[k][j];
            data[k][j] = temp;
        }
    }

    T& operator()(int row, int col) {
        return data.at(row).at(col);
    }

    vector<T>& operator()(int row) {
        return data.at(row);
    }

    vector<vector<T>>& operator()() {
        return data;
    }

    size_t size() const {
        return data.size();
    }

    void print() const {
        for (size_t i = 0; i < data.size(); ++i) {
            for (size_t j = 0; j < data[i].size(); ++j){
                cout << data.at(i).at(j) << '\t';
            }
            cout << endl;
        }
        cout << endl;
    }

};

class FileIO {
public:
    static void loadDataFromFile(Matrix<double>& matrix, const string filename) {
        ifstream file(filename);
        size_t count = 0;
        vector<vector<double>> arr;

        if (file.is_open()) {
            string line;
            while (getline(file, line)) {
                arr.push_back(vector<double>());
                stringstream split(line);
                double coef;
                while (split >> coef) {
                    arr.back().push_back(coef);
                }
                ++count;
            }
        } else {
            cout << "Cannot open file: " << filename << "\nExiting..." << endl;
            exit(0);
        }
        matrix = arr;
        file.close();
    }

    static void writeResultToFile(vector<double> x) {
        ofstream file("X.txt");

        if (file.is_open()) {
            for (size_t i = 0; i < x.size(); ++i) {
                file << x.at(i) << endl;
            }
        } else {
            cout << "Cannot open file: X.txt!\nExiting..." << endl;
            exit(0);
        }
        file.close();
    }
};

class MatrixCalculator {
public:
    static double calculateDeterminant(Matrix<double> matrix, size_t n) {
        Matrix<double> temp{matrix.size()};

        double result = 0.0;
        if (n == 2) {
            return((matrix(0, 0) * matrix(1,1)) - (matrix(1, 0) * matrix(0, 1)));
        }
        else {
            for(size_t c = 0; c < n; ++c) {
                size_t subi = 0;
                for(size_t i = 1; i < n; ++i) {
                    size_t subj = 0;
                    for(size_t j = 0; j < n; j++) {
                        if (j == c) continue;
                        temp(subi, subj) = matrix(i, j);
                        ++subj;
                    }
                    ++subi;
                }
                result = result + (pow(-1, c) * matrix(0, c) * calculateDeterminant(temp, n - 1));
            }
        }
        return result;
    }

    static void pivot(Matrix<double>& matrix) {
        const size_t size = matrix.size();
        for (size_t i = 0; i < size; ++i){
            for (size_t k = i + 1; k < size; ++k){
                if (matrix(i, i) < matrix(k, i)) {
                    matrix.swap(i, k);
                }
            }
        }
    }

    static void eliminate(Matrix<double>& matrix) {
        const size_t size = matrix.size();
        for (size_t i = 0; i < size - 1; ++i){
            for (size_t k = i + 1; k < size; ++k){
                double t = matrix(k, i) / matrix(i, i);
                for (size_t j = 0; j <= size; ++j) {
                    matrix(k, j) = matrix(k, j) - t * matrix(i, j);
                }
            }
        }
    }

    static vector<double> backSubstitution(Matrix<double>& matrix) {
        const size_t size = matrix.size();
        vector<double> x(size);

        for (int i = size - 1; i >= 0; --i) {
            x.at(i) = matrix(i, size);
            for (size_t j = 0; j < size; ++j){
                if (j != i) {
                    x.at(i) = x.at(i) - matrix(i, j) * x.at(j);
                }
            }
            x.at(i) = x.at(i) / matrix(i, i);
        }
        return x;
    }

    static double calculateConditionNumber(Matrix<double> matrix, double determinant, int calcInf) {
        Matrix<double> inverse = MatrixCalculator::calculateInverse(matrix, determinant);
        double conditionOfOrig, conditionOfInverse;
        if (calcInf) {
            conditionOfOrig = calculateConditionInfinity(matrix);
            conditionOfInverse = calculateConditionInfinity(inverse);
        }
        else {
            conditionOfOrig = calculateConditionOne(matrix);
            conditionOfInverse = calculateConditionOne(inverse);
        }
        return conditionOfOrig * conditionOfInverse;
    }

    static double calculateConditionInfinity(Matrix<double> matrix) {
        const double sumOfFirstRow = abs(matrix(0, 0)) + abs(matrix(0, 1));
        const double sumOfSecondRow = abs(matrix(1, 0)) + abs(matrix(1, 1));
        return getBigger(sumOfFirstRow, sumOfSecondRow);
    }

    static double calculateConditionOne(Matrix<double> matrix) {
        const double sumOfFirstCol = abs(matrix(0, 0)) + abs(matrix(1, 0));
        const double sumOfSecondCol = abs(matrix(0, 1)) + abs(matrix(1, 1));
        return getBigger(sumOfFirstCol, sumOfSecondCol);
    }

    static double getBigger(double first, double second) {
        if (first > second) return first;
        else return second;
    }

    static Matrix<double> calculateInverse(Matrix<double> matrix, double determinant) {
        Matrix<double> inverse{2};
        inverse(0, 0) = matrix(1, 1) / determinant;
        inverse(0, 1) = -1 * matrix(0, 1) / determinant;
        inverse(1, 0) = -1 * matrix(1, 0) / determinant;
        inverse(1, 1) = matrix(0, 0) / determinant;
        return inverse;
    }

};

int main() {
    cout.precision(4);
    cout.setf(ios::fixed);
    Matrix<double> first{};
    Matrix<double> second{};
    FileIO::loadDataFromFile(first, "A.txt");
    FileIO::loadDataFromFile(second, "B.txt");
    const double determinant = MatrixCalculator::calculateDeterminant(first, first.size());;
    if (determinant == 0) {
        cout << "Matrix in A.txt is singular. Exiting..." << endl;
        exit(0);
    } else {
        if (first.size() == 2) {
            const double conditionInf = MatrixCalculator::calculateConditionNumber(first, determinant, 1);
            const double conditionOne = MatrixCalculator::calculateConditionNumber(first, determinant, 0);
            cout << "conditionInf: " << conditionInf << endl;
            cout << "conditionOne: " << conditionOne << endl;
        }
        Matrix<double> augmented{first, second};
        MatrixCalculator::pivot(augmented);
        MatrixCalculator::eliminate(augmented);
        const vector<double> result = MatrixCalculator::backSubstitution(augmented);
        FileIO::writeResultToFile(result);
    }

    return 0;
}

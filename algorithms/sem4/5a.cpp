#include <bits/stdc++.h>
#include <cmath>
#include <vector>

using namespace std;

#define PI 3.14159265358979323846

struct complexNumber {
    double real, img;

    complexNumber(double r, double i)
        : real(r)
        , img(i) {
    }

    explicit complexNumber(double phi)
		: real(cos(phi))
		, img(sin(phi))
    {}

    [[nodiscard]] complexNumber conjugate() const {
        return {real, -img};
    }

    [[nodiscard]] double normSquared() const {
        return real * real + img * img;
    }

    complexNumber operator+(const complexNumber& num) const {
        return {real + num.real, img + num.img};
    }

    complexNumber operator+(const double& num) const {
        return {real + num, img};
    }


    complexNumber operator*(const complexNumber& num) const {
		return {real * num.real - img * num.img, real * num.img + img * num.real};
    }

    complexNumber operator*(const double& num) const {
        return {real * num, img * num};
    }

    complexNumber operator-(const complexNumber& num) const {
        return {real - num.real, img - num.img};
    }

    complexNumber operator-(const double& num) const {
        return {real - num, img};
    }

    complexNumber operator/(const double& num) const {
        return {real / num, img / num};
    }
};

vector<int> convertToPolynomial(string m) {
    vector<int> poly(m.length());
    for (int i = 0; i < m.length(); i++) {
        poly[i] = m[m.length() - i - 1] - '0';
    }
    return poly;
}

vector<complexNumber> complexArray(vector<int>& a, int size) {
    vector<complexNumber> res;
    for (int i = 0; i < size; i++) {
        if (i < a.size()) {
            res.emplace_back(a[i], 0);
        } else {
            res.emplace_back(0, 0);
        }
    }
    return res;
}

long biggestPower2(int a) {
    int n = 1;
    while (n < a) {
        n = n << 1;
    }
    return n << 1;
}

void fft(vector<complexNumber>& polynom, bool inverse) {
    int size = polynom.size();
    if (size == 1)
        return;

    vector<complexNumber> even;
    vector<complexNumber> odd;

    auto zero = complexNumber(0.0, 0.0);
    even.reserve(size / 2);
    odd.reserve(size / 2);
    for (int i = 0; i < size / 2 ; ++i) {
        even.push_back(zero);
        odd.push_back(zero);
    }

    int index = 0;
    for (int i = 0; i < size; i += 2) {
        even[index] = polynom[i];
        odd[index++] = polynom[i + 1];
    }

    fft(even, inverse);
    fft(odd, inverse);

    double phi;
    if (!inverse) {
        phi = 2 * PI / size;
    } else {
        phi = -1 * 2 * PI / size;
    }

    complexNumber w = complexNumber(1.0, 0.0);
    auto wByN = complexNumber(phi);
    for (int i = 0; i < size / 2; i++) {
        polynom[i] = even[i] + w * odd[i];
        polynom[i + size / 2] = even[i] - w * odd[i];
        if (inverse) {
            polynom[i] = polynom[i] / 2;
            polynom[i + size / 2] = polynom[i + size / 2] / 2;
        }
        w = w * wByN;
    }
}

vector<int> timesFFT(vector<int>& poly1, vector<int>& poly2) {
    vector<int> res;
    int n = biggestPower2(max(poly1.size(), poly2.size()));
    auto cPoly1 = complexArray(poly1, n);
    auto cPoly2 = complexArray(poly2, n);

    fft(cPoly1, false);
    fft(cPoly2, false);

    for (int i = 0; i < n; i++) {
        cPoly1[i] = cPoly1[i] * cPoly2[i];
    }

    fft(cPoly1, true);
    res.reserve(n);
    for (int i = 0; i < n;  i++) {
        res.push_back(static_cast<int>(cPoly1[i].real + 0.5));
    }
    return res;
}

vector<int> normalize(vector<int>& a) {
    int car = 0;
    for (int & i : a) {
        i += car;
        car = i / 10;
        i = i % 10;
    }
    return a;
}

void getNumberFromArray(vector<int>& a, int pos1, int pos2) {
    int pom = pos1 * pos2;
    if (pom == -1) {
        cout << "-";
    }
    int flag = 0;
    for (int i = a.size() - 1; i >= 0; i--) {
        if (a[i] != 0) {
            flag++;
        }
        if (flag > 0) {
            cout << a[i];
        }
    }
}

string positive(string& a) {
    if (a[0] == '-') {
        return a.erase(0, 1);
    }
    return a;
}

int main() {
    string n, m;
    cin >> n;
    cin >> m;

    if ((n.size() == 1 && n == "0") || (m.size() == 1 && m == "0")) {
        cout << 0;
        return 0;
    }

    int pos1, pos2;

    if (n[0] == '-') {
        pos1 = -1;
    } else {
        pos1 = 1;
    }
    if (m[0] == '-') {
        pos2 = -1;
    } else {
        pos2 = 1;
    }

    auto a = convertToPolynomial(positive(n));
    auto b = convertToPolynomial(positive(m));
    

    auto times = timesFFT(a, b);
    auto normalTimes = normalize(times);

    getNumberFromArray(normalTimes, pos1, pos2);
    return 0;
}


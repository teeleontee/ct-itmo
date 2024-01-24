#include <array>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>
#include <math.h>
#include <memory>
#include <sstream>
#include <string_view>
#include <vector>

const double G = (6.67 * std::pow(10, -11));
const double theta = 0.5;

struct Cartesian
{
    Cartesian(double x, double y);
    double x;
    double y;

    friend Cartesian operator+(const Cartesian & lhs, const Cartesian & rhs);

    friend Cartesian operator-(const Cartesian & lhs, const Cartesian & rhs);

    friend Cartesian operator*(const Cartesian & lhs, const double & rhs);

    friend Cartesian operator/(const Cartesian & lhs, const double & rhs);

    Cartesian();
};

// Quadrant representation, required for Problem 2
class Quadrant
{
public:
    // Create quadrant with center (x, y) and size 'length'
    Quadrant(Cartesian center, double length)
        : _center(center)
        , _radius(length)
    {
    }

    // Test if point (x, y) is in the quadrant
    bool contains(Cartesian p) const;

    double length() const
    {
        return _radius;
    }

    // The four methods below construct new Quadrant representing sub-quadrant of the invoking quadrant
    Quadrant nw() const;

    Quadrant ne() const;

    Quadrant sw() const;

    Quadrant se() const;

    friend std::ostream & operator<<(std::ostream & strm, const Quadrant & quad)
    {
        strm << "quad center: (" << quad._center.x << ", " << quad._center.y << ") , quad length: " << quad._radius << std::endl;
        return strm;
    }

private:
    Cartesian _center;
    double _radius;
};

// Single body representation, required for Problem 1 and Problem 2
class Body
{
public:
    Body();

    Body(const std::string & name, const Cartesian & force, const Cartesian & speed, const Cartesian & coords, double mass);

    double distance(const Body & b) const;

    // calculate the force-on current body by the 'b' and add the value to accumulated force value
    void add_force(const Body & b);

    // reset accumulated force value
    void reset_force();

    Cartesian get_acceleration() const;

    Cartesian get_coordinates() const { return coords; }

    // update body's velocity and position
    void update(double delta_t);

    friend std::ostream & operator<<(std::ostream &, const Body &);

    const std::string & getName() const;

    friend std::istream & operator>>(std::istream &, Body & body);

    // Test if body is in quadrant
    bool in(Quadrant quadrant);

    // Create new body representing center-of-mass of the invoking body and 'b'
    Body plus(const Body & b) const;

private:
    std::string name;
    Cartesian force;
    Cartesian speed;
    Cartesian coords;
    double mass;
};

// Burnes-Hut tree representation, required for Problem 2
class BHTreeNode
{
private:
    Quadrant _quad;
    std::shared_ptr<Body> _body;
    std::array<std::shared_ptr<BHTreeNode>, 4> _kids;
    bool is_leaf = true;

public:
    BHTreeNode(const Quadrant & field)
        : _quad(field)
    {
    }

    void insert(Body && b);

    // Update net acting force-on 'b'
    void update_force(Body & b);
};

using Track = std::vector<Cartesian>;

class PositionTracker
{
protected:
    PositionTracker(const std::string & filename);

public:
    virtual ~PositionTracker() = default;

protected:
    std::vector<Body> bodies;
    double galaxy_size;

public:
    virtual Track track(const std::string & body_name, size_t end_time, size_t time_step) = 0;
};

class BasicPositionTracker : public PositionTracker
{
public:
    BasicPositionTracker(const std::string & filename)
        : PositionTracker(filename)
    {
    }

    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;
};

class FastPositionTracker : public PositionTracker
{
public:
    FastPositionTracker(const std::string & filename)
        : PositionTracker(filename)
    {
    }

    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;
};
#include "nbody.h"

#include <algorithm>
#include <fstream>
#include <string>

// ------------- Cartesian ---------------
Cartesian::Cartesian(double x, double y)
    : x(x)
    , y(y)
{
}
Cartesian::Cartesian()
    : x(0)
    , y(0)
{
}
Cartesian operator/(const Cartesian & lhs, const double & rhs)
{
    return Cartesian(lhs.x / rhs, lhs.y / rhs);
}

Cartesian operator+(const Cartesian & lhs, const Cartesian & rhs)
{
    return Cartesian(lhs.x + rhs.x, lhs.y + rhs.y);
}

Cartesian operator-(const Cartesian & lhs, const Cartesian & rhs)
{
    return Cartesian(lhs.x - rhs.x, lhs.y - rhs.y);
}

Cartesian operator*(const Cartesian & lhs, const double & rhs)
{
    return Cartesian(lhs.x * rhs, lhs.y * rhs);
}

// ---------------- Body -----------------
Body::Body(const std::string & name, const Cartesian & force, const Cartesian & speed, const Cartesian & coords, double mass)
    : name(name)
    , force(force)
    , speed(speed)
    , coords(coords)
    , mass(mass)
{
}

double Body::distance(const Body & b) const
{
    double sqrd_sum = (this->coords.x - b.coords.x) * (this->coords.x - b.coords.x) + (this->coords.y - b.coords.y) * (this->coords.y - b.coords.y);
    return std::sqrt(sqrd_sum);
}

void Body::add_force(const Body & b)
{
    double dist_b = distance(b);
    double just_f = G * b.mass * this->mass / (dist_b * dist_b);
    this->force.x += just_f * (this->coords.x - b.coords.x) / dist_b;
    this->force.y += just_f * (this->coords.y - b.coords.y) / dist_b;
}

void Body::reset_force()
{
    this->force = Cartesian();
}

Cartesian Body::get_acceleration() const
{
    return this->force / mass;
}

std::ostream & operator<<(std::ostream & strm, const Body & b)
{
    strm << " " << b.coords.x << " " << b.coords.y << " " << b.speed.x << " " << b.speed.y << " " << b.mass << " " << b.name << "\n";
    return strm;
}

const std::string & Body::getName() const
{
    return name;
}

void Body::update(double delta_t)
{
    auto acc = this->get_acceleration();
    this->speed = this->speed + (acc * delta_t);
    this->coords = this->coords + (this->speed * delta_t);
}

bool Body::in(const Quadrant quadrant)
{
    if (quadrant.contains(coords)) {
        return true;
    }
    return false;
}

Body Body::plus(const Body & b) const
{
    double new_body_mass = b.mass + this->mass;
    auto new_coords = (b.coords * b.mass + coords * mass) / new_body_mass;
    return Body("inner", Cartesian(), Cartesian(), new_coords, new_body_mass);
}

std::istream & operator>>(std::istream & strm, Body & body)
{
    strm >> body.coords.x >> body.coords.y >> body.speed.x >> body.speed.y >> body.mass >> body.name;
    body.force = Cartesian();
    return strm;
}

Body::Body()
    : name("")
    , force(Cartesian())
    , speed(Cartesian())
    , coords(Cartesian())
    , mass(0)
{
}

// ---------- BHTreeNode ------------
void BHTreeNode::insert(Body && b)
{
    if (_body == nullptr) {
        _body = std::make_shared<Body>(b);
        return;
    }
    auto & temp = b;
    if (is_leaf) {
        _kids[0] = std::make_shared<BHTreeNode>(_quad.nw());
        _kids[1] = std::make_shared<BHTreeNode>(_quad.ne());
        _kids[2] = std::make_shared<BHTreeNode>(_quad.sw());
        _kids[3] = std::make_shared<BHTreeNode>(_quad.se());
        is_leaf = false;
        for (auto kid : _kids) {
            if (kid->_quad.contains(b.get_coordinates())) {
                kid->insert(std::move(b));
                break;
            }
        }
        for (auto kid : _kids) {
            if (kid->_quad.contains(_body->get_coordinates())) {
                kid->insert(std::move(*_body));
                break;
            }
        }
    }
    else {
        for (auto kid : _kids) {
            if (kid->_quad.contains(b.get_coordinates())) {
                kid->insert(std::move(b));
                break;
            }
        }
    }
    _body = std::make_shared<Body>(_body->plus(temp));
}

// Update net acting force-on 'b'
void BHTreeNode::update_force(Body & b)
{
    if (!this->_body) {
        return;
    }
    if (is_leaf && b.getName() != this->_body->getName()) {
        b.add_force(*this->_body);
    }
    else if (!is_leaf) {
        if (_quad.length() * 2 / this->_body->distance(b) < theta) {
            b.add_force(*this->_body);
        }
        else {
            for (auto kid : _kids) {
                kid->update_force(b);
            }
        }
    }
}

// --------- Quadrant -----------
bool Quadrant::contains(Cartesian p) const
{
    if (p.x <= _center.x + _radius && p.x >= _center.x - _radius &&
        p.y <= _center.y + _radius && p.y >= _center.y - _radius) {
        return true;
    }
    return false;
}

Quadrant Quadrant::nw() const
{
    Cartesian max_nw(_center.x - _radius, _center.y + _radius);
    double new_radius = _radius / 2;
    Cartesian new_center((max_nw.x + _center.x) / 2, (max_nw.y + _center.y) / 2);
    return Quadrant(new_center, new_radius);
}

Quadrant Quadrant::ne() const
{
    Cartesian max_ne(_center.x + _radius, _center.y + _radius);
    double new_radius = _radius / 2;
    Cartesian new_center((max_ne.x + _center.x) / 2, (max_ne.y + _center.y) / 2);
    return Quadrant(new_center, new_radius);
}

Quadrant Quadrant::sw() const
{
    Cartesian max_sw(_center.x - _radius, _center.y - _radius);
    double new_radius = _radius / 2;
    Cartesian new_center((max_sw.x + _center.x) / 2, (max_sw.y + _center.y) / 2);
    return Quadrant(new_center, new_radius);
}

Quadrant Quadrant::se() const
{
    Cartesian max_se(_center.x + _radius, _center.y - _radius);
    double new_radius = _radius / 2;
    Cartesian new_center((max_se.x + _center.x) / 2, (max_se.y + _center.y) / 2);
    return Quadrant(new_center, new_radius);
}

// ----------- Trackers -----------
PositionTracker::PositionTracker(const std::string & filename)
{
    std::ifstream myfile(filename);
    myfile >> galaxy_size;
    std::istream_iterator<Body> it1(myfile);
    std::istream_iterator<Body> eos;
    std::vector<Body> vec(it1, eos);
    bodies = std::move(vec);
}

Track BasicPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track res;
    auto it = std::find_if(bodies.begin(), bodies.end(), [&body_name](Body & b) { return b.getName() == body_name; });
    std::vector<Body> new_bodies(bodies.size());
    for (size_t time = 0; time < end_time; time += time_step) {
        res.emplace_back(it->get_coordinates());
        for (size_t i = 0; i < bodies.size(); i++) {
            new_bodies[i] = bodies[i];
            new_bodies[i].reset_force();
            for (size_t j = 0; j < bodies.size(); ++j) {
                if (i == j) {
                    continue;
                }

                new_bodies[i].add_force(bodies[j]);
            }
            new_bodies[i].update(time_step);
        }
        std::swap(bodies, new_bodies);
    }
    res.emplace_back(it->get_coordinates());
    return res;
}

Track FastPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track res;
    auto it = std::find_if(bodies.begin(), bodies.end(), [&body_name](Body & b) { return b.getName() == body_name; });
    for (std::size_t time = 0; time < end_time; time += time_step) {
        Quadrant gs = Quadrant(Cartesian(), galaxy_size);
        BHTreeNode tree = BHTreeNode(gs);
        for (std::size_t i = 0; i < bodies.size(); ++i) {
            tree.insert(std::move(bodies[i]));
        }
        res.push_back(it->get_coordinates());
        for (auto & body : bodies) {
            body.reset_force();
            tree.update_force(body);
            body.update(time_step);
        }
    }
    res.emplace_back(it->get_coordinates());
    return res;
}

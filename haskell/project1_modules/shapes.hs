-- Module Shapes
-- This module calculates the area and the volume of several geometric
-- shapes like a sphere, a cylinder, a cone, etc..
--
-- In parentesis is defined the functions and the types that
-- will be exported by this module.
--
-- :author: Caique Marques
module Shapes   (
                    Shape(
                        Sphere, Cylinder, Cone, Frustum,
                        OblateSpheroid, ProlateSpheroid
                        ),
                    Radius, Height, SemiMajorAxis, SemiMinorAxis, 
                    area, volume
                ) where

-- The Shape can be a sphere or a cylinder or a cone and so on.
-- Each shape has one or more important thing to calculate their
-- area and/or their volume, so, it is must be declared. For example
-- to calculate the area and the volume of a sphere, is necessary the
-- radius, so, it declared as part of the sphere.
-- "Show" converts types to be showed in the characteres forms.
data Shape = Sphere Radius
    | Cylinder Height Radius
    | Cone Height Radius
    | Frustum Height Radius Radius
    | OblateSpheroid SemiMajorAxis SemiMinorAxis
    | ProlateSpheroid SemiMajorAxis SemiMinorAxis
    deriving Show

-- Defining types for the metrics
type Radius         = Float
type Height         = Float
type SemiMajorAxis  = Float
type SemiMinorAxis  = Float

-----------------------------
--       Functions       ----
-----------------------------

-- Calculates and returns the area of several shapes, specified
-- in the parameter "Shape" of the function.
--
-- :param Shape: The shape desired to calculate the area.
-- :rtype: Float.
area :: Shape -> Float
area(Sphere r) =
    4 * pi * r^2
area(Cylinder height radius) =
    let side_area = 2 * pi + radius^2
        top_area = pi * radius * height
    in side_area + 2 * top_area
area(Cone height radius) =
    let side_area = pi * radius * sqrt(radius^2 + height^2)
        circle_area = pi * radius^2
    in side_area + circle_area
area(Frustum h r1 r2) =
    let side_area = pi * (r1+r2) * sqrt(h^2 + (r1-r2)^2)
    in pi * r1^2 + pi * r2^2 + side_area
area(OblateSpheroid a b) =
    let e = sqrt(a^2 + b^2)/a
    in 2 * pi * a^2 + (b^2/e) * log((1+e)/(1-e))
area(ProlateSpheroid a b) =
    let e = sqrt(a^2 + b^2)/a
    in 2 * pi * b^2 + 2 * pi * ((a * b)/e) * asin(e)

-- Calculates and returns the volume of several shapes, specified
-- in the parameter "Shape" of the function
--
-- :param Shape: The shape desired to calculate the volume.
-- :rtype: Float.
volume :: Shape -> Float
volume(Sphere r)            = 4 * pi * r^2
volume(Cylinder h r)        = pi * r^2 * h
volume(Cone h r)            = (1/3) * pi * r^2 * h
volume(Frustum h r1 r2)     = (1/3) * pi * h * (r1^2 + r2^2 + r1 * r2)
volume(OblateSpheroid a b)  = (4/3) * pi * a^2 * b
volume(ProlateSpheroid a b) = (4/3) * pi * a * b^2

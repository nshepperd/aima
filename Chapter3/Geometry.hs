{-# LANGUAGE UnicodeSyntax #-}
module Chapter3.Geometry (Vertex, Polygon, LineSegment, p_intersects) where

type Vertex a = (a, a)
type Polygon a = [Vertex a]
type LineSegment a = (Vertex a, Vertex a)

addv :: (Fractional a) => Vertex a -> Vertex a -> Vertex a
addv (x1,y1) (x2,y2) = (x1+x2, y1+y2)
subv :: (Fractional a) => Vertex a -> Vertex a -> Vertex a
subv (x1,y1) (x2,y2) = (x1-x2, y1-y2)
mulv :: (Fractional a) => a -> Vertex a -> Vertex a
mulv a (x, y) = (a*x, a*y)
-- "Cross" product
(×) :: (Fractional a) => Vertex a -> Vertex a -> a
(×) (x1,y1) (x2, y2) = (x1*y2 - y1*x2)

poly_to_lines :: Polygon a -> [LineSegment a]
poly_to_lines xs = init $ scanr (\p (q, _) -> (p, q)) (head xs, head xs) xs

clamp :: (Num a, Ord a) => a -> a
clamp x = min (max x 0) 1

left_half_plane :: (Fractional a, Ord a) => LineSegment a -> LineSegment a -> LineSegment a
left_half_plane seg@(p, pr) (q, qs) = if r × s /= 0 then
                                        -- relative slant
                                        let t = clamp $ ((q `subv` p) × s) / (r × s) in
                                         if r × s < 0 then
                                           -- left slant, everything after t is in the left plane
                                           ((p `addv` mulv t r), pr)
                                         else
                                           -- right slant, everything before t
                                           (p, (p `addv` mulv t r))
                                      else
                                        -- collinear
                                        if (p `subv` q) × s < 0 then
                                          -- p is left of the dividing line, so the whole line segment is in the interior
                                          seg
                                        else
                                          -- otherwise, none of it intersects the interior
                                          (p, p)
  where
    r = pr `subv` p
    s = qs `subv` q

-- Finding the intersection of a line segment with a convex polygon.
-- We do this by iteratively taking the intersection with the open half-plane defined by each edge.
-- The intersection of all such half-planes is the interior of the polygon.
p_intersection :: (Fractional a, Ord a) => LineSegment a -> Polygon a -> LineSegment a
p_intersection seg poly = foldl left_half_plane seg (poly_to_lines poly)


p_intersects :: (Fractional a, Ord a) => LineSegment a -> Polygon a -> Bool
p_intersects seg poly = p /= q
    where (p,q) = p_intersection seg poly

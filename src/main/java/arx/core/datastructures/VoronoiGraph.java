package arx.core.datastructures;
/*
* The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
* Bell Laboratories.
* Permission to use, copy, modify, and distribute this software for any
* purpose without fee is hereby granted, provided that this entire notice
* is included in all copies of any software which is or includes a copy
* or modification of this software and in all copies of the supporting
* documentation for such software.
* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
*/

/*
* This code was originally written by Stephan Fortune in C code.  I, Shane O'Sullivan,
* have since modified it, encapsulating it in a C++ class and, fixing memory leaks and
* adding accessors to the Voronoi Edges.
* Permission to use, copy, modify, and distribute this software for any
* purpose without fee is hereby granted, provided that this entire notice
* is included in all copies of any software which is or includes a copy
* or modification of this software and in all copies of the supporting
* documentation for such software.
* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
*/

/*
* Java Version by Zhenyu Pan
* Permission to use, copy, modify, and distribute this software for any
* purpose without fee is hereby granted, provided that this entire notice
* is included in all copies of any software which is or includes a copy
* or modification of this software and in all copies of the supporting
* documentation for such software.
* THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
* REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
* OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
*/

/*
 * Tweaking for personal usage
 */

import arx.core.vec.ReadVec2f;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Halfedge {
    Halfedge ELleft, ELright;

    VornoiEdge ELedge;

    boolean deleted;

    int ELpm;

    Site vertex;

    float ystar;

    Halfedge PQnext;

    Halfedge() {
        PQnext = null;
    }
}

public class VoronoiGraph {

    float borderMinX, borderMaxX, borderMinY, borderMaxY;

    int siteidx;

    float xmin, xmax, ymin, ymax, deltax, deltay;

    int nvertices;

    int nedges;

    int nsites;

    Site[] sites;

    Site bottomsite;

    int sqrt_nsites;

    float minDistanceBetweenSites;

    int PQcount;

    int PQmin;

    int PQhashsize;

    Halfedge PQhash[];

    public static int le = 0;

    public static int re = 1;

    int ELhashsize;

    Halfedge ELhash[];

    Halfedge ELleftend, ELrightend;

    List<GraphEdge> allEdges = new ArrayList<>();

    List<ReadVec2f> allPoints = new ArrayList<>();

    Map<ReadVec2f,Integer> pointIndices = new HashMap<>();

    public VoronoiGraph() {
        siteidx = 0;
        sites = null;

        minDistanceBetweenSites = 0;
    }

    void cleanupSites() {
        sites = null;
    }

    void cleanupEdges() {
        allEdges.clear();
    }

    void dVoronoiDiagramGenerator() {
        cleanupSites();
        cleanupEdges();
    }

    int scomp(Site s1, Site s2) {
        if (s1.y < s2.y)
            return (-1);
        if (s1.y > s2.y)
            return (1);
        if (s1.x < s2.x)
            return (-1);
        if (s1.x > s2.x)
            return (1);
        return (0);
    }

    public void sortNode(float xValues[], float yValues[], int numPoints) {
        int i;
        nsites = numPoints;
        sites = new Site[nsites];
        xmin = xValues[0];
        ymin = yValues[0];
        xmax = xValues[0];
        ymax = yValues[0];
        for (i = 0; i < nsites; i++) {
            sites[i] = new Site();
            sites[i].x = xValues[i];
            sites[i].y = yValues[i];
            sites[i].sitenbr = i;

            if (xValues[i] < xmin)
                xmin = xValues[i];
            else if (xValues[i] > xmax)
                xmax = xValues[i];

            if (yValues[i] < ymin)
                ymin = yValues[i];
            else if (yValues[i] > ymax)
                ymax = yValues[i];
        }

        Arrays.sort(sites, this::scomp);

        deltay = ymax - ymin;
        deltax = xmax - xmin;
    }

    /* return a single in-storage site */
    Site nextone() {
        Site s;
        if (siteidx < nsites) {
            s = sites[siteidx];
            siteidx += 1;
            return (s);
        } else
            return (null);
    }

    VornoiEdge bisect(Site s1, Site s2) {
        float dx, dy, adx, ady;
        VornoiEdge newedge;

        newedge = new VornoiEdge();

        // store the sites that this edge is bisecting
        newedge.reg0 = s1;
        newedge.reg1 = s2;
        // to begin with, there are no endpoints on the bisector - it goes to
        // infinity
        newedge.ep0 = null;
        newedge.ep1 = null;

        // get the difference in x dist between the sites
        dx = s2.x - s1.x;
        dy = s2.y - s1.y;
        // make sure that the difference in positive
        adx = dx > 0 ? dx : -dx;
        ady = dy > 0 ? dy : -dy;
        newedge.c = (float) (s1.x * dx + s1.y * dy + (dx * dx + dy
                * dy) * 0.5);// get the slope of the line

        if (adx > ady) {
            newedge.a = 1.0f;
            newedge.b = dy / dx;
            newedge.c /= dx;// set formula of line, with x fixed to 1
        } else {
            newedge.b = 1.0f;
            newedge.a = dx / dy;
            newedge.c /= dy;// set formula of line, with y fixed to 1
        }

        newedge.edgenbr = nedges;

        nedges += 1;
        return (newedge);
    }

    public void initializePoints(float[] xValues, float[] yValues) {
        assert(xValues.length == yValues.length);
        final int count = xValues.length;
        dVoronoiDiagramGenerator();

        nsites = xValues.length;
        minDistanceBetweenSites = 0;
        nvertices = 0;
        nedges = 0;

        float sn = (float) nsites + 4;
        sqrt_nsites = (int) Math.sqrt(sn);

        sortNode(xValues, yValues, count);
    }

    void makevertex(Site v) {
        v.sitenbr = nvertices;
        nvertices += 1;
    }

    boolean PQinitialize() {
        PQcount = 0;
        PQmin = 0;
        PQhashsize = 4 * sqrt_nsites;
        PQhash = new Halfedge[PQhashsize];

        for (int i = 0; i < PQhashsize; i += 1)
            PQhash[i] = new Halfedge();
        return true;
    }

    int PQbucket(Halfedge he) {
        int bucket;

        bucket = (int) ((he.ystar - ymin) / deltay * PQhashsize);
        if (bucket < 0)
            bucket = 0;
        if (bucket >= PQhashsize)
            bucket = PQhashsize - 1;
        if (bucket < PQmin)
            PQmin = bucket;
        return (bucket);
    }

    // push the HalfEdge into the ordered linked list of vertices
    void PQinsert(Halfedge he, Site v, float offset) {
        Halfedge last, next;

        he.vertex = v;
        he.ystar = (float) (v.y + offset);
        last = PQhash[PQbucket(he)];
        while ((next = last.PQnext) != null
                && (he.ystar > next.ystar || (he.ystar == next.ystar && v.x > next.vertex.x))) {
            last = next;
        }
        he.PQnext = last.PQnext;
        last.PQnext = he;
        PQcount += 1;
    }

    // remove the HalfEdge from the list of vertices
    void PQdelete(Halfedge he) {
        Halfedge last;

        if (he.vertex != null) {
            last = PQhash[PQbucket(he)];
            while (last.PQnext != he)
                last = last.PQnext;

            last.PQnext = he.PQnext;
            PQcount -= 1;
            he.vertex = null;
        }
    }

    boolean PQempty() {
        return (PQcount == 0);
    }

    Point PQ_min(Point answer) {
        while (PQhash[PQmin].PQnext == null) {
            PQmin += 1;
        }
        answer.x = PQhash[PQmin].PQnext.vertex.x;
        answer.y = PQhash[PQmin].PQnext.ystar;
        return answer;
    }

    Halfedge PQextractmin() {
        Halfedge curr;

        curr = PQhash[PQmin].PQnext;
        PQhash[PQmin].PQnext = curr.PQnext;
        PQcount -= 1;
        return (curr);
    }

    Halfedge HEcreate(VornoiEdge e, int pm) {
        Halfedge answer;
        answer = new Halfedge();
        answer.ELedge = e;
        answer.ELpm = pm;
        answer.PQnext = null;
        answer.vertex = null;
        return (answer);
    }

    boolean ELinitialize() {
        int i;
        ELhashsize = 2 * sqrt_nsites;
        ELhash = new Halfedge[ELhashsize];

        for (i = 0; i < ELhashsize; i += 1)
            ELhash[i] = null;
        ELleftend = HEcreate(null, 0);
        ELrightend = HEcreate(null, 0);
        ELleftend.ELleft = null;
        ELleftend.ELright = ELrightend;
        ELrightend.ELleft = ELleftend;
        ELrightend.ELright = null;
        ELhash[0] = ELleftend;
        ELhash[ELhashsize - 1] = ELrightend;

        return true;
    }

    Halfedge ELright(Halfedge he) {
        return (he.ELright);
    }

    Halfedge ELleft(Halfedge he) {
        return (he.ELleft);
    }

    Site leftreg(Halfedge he) {
        if (he.ELedge == null)
            return (bottomsite);
        return (he.ELpm == le ? he.ELedge.reg0 : he.ELedge.reg1);
    }

    void ELinsert(Halfedge lb, Halfedge newHe) {
        newHe.ELleft = lb;
        newHe.ELright = lb.ELright;
        (lb.ELright).ELleft = newHe;
        lb.ELright = newHe;
    }

    /*
     * This delete routine can't reclaim node, since pointers from hash table
     * may be present.
     */
    void ELdelete(Halfedge he) {
        (he.ELleft).ELright = he.ELright;
        (he.ELright).ELleft = he.ELleft;
        he.deleted = true;
    }

    /* Get entry from hash table, pruning any deleted nodes */
    Halfedge ELgethash(int b) {
        Halfedge he;

        if (b < 0 || b >= ELhashsize)
            return (null);
        he = ELhash[b];
        if (he == null || !he.deleted)
            return (he);

		/* Hash table points to deleted half edge. Patch as necessary. */
        ELhash[b] = null;
        return (null);
    }

    Halfedge ELleftbnd(float x, float y) {
        int i, bucket;
        Halfedge he;

		/* Use hash table to get close to desired halfedge */
        // use the hash function to find the place in the hash map that this
        // HalfEdge should be
        bucket = (int) ((x - xmin) / deltax * ELhashsize);

        // make sure that the bucket position in within the range of the hash
        // array
        if (bucket < 0)
            bucket = 0;
        if (bucket >= ELhashsize)
            bucket = ELhashsize - 1;

        he = ELgethash(bucket);
        if (he == null)
        // if the HE isn't found, search backwards and forwards in the hash map
        // for the first non-null entry
        {
            for (i = 1; i < ELhashsize; i += 1) {
                if ((he = ELgethash(bucket - i)) != null)
                    break;
                if ((he = ELgethash(bucket + i)) != null)
                    break;
            }
        }
		/* Now search linear list of halfedges for the correct one */
        if (he == ELleftend || (he != ELrightend && right_of(he, x, y))) {
            // keep going right on the list until either the end is reached, or
            // you find the 1st edge which the point isn't to the right of
            do {
                he = he.ELright;
            } while (he != ELrightend && right_of(he, x,y));
            he = he.ELleft;
        } else
            // if the point is to the left of the HalfEdge, then search left for
            // the HE just to the left of the point
            do {
                he = he.ELleft;
            } while (he != ELleftend && !right_of(he, x,y));

		/* Update hash table and reference counts */
        if (bucket > 0 && bucket < ELhashsize - 1) {
            ELhash[bucket] = he;
        }
        return (he);
    }

    void pushGraphEdge(float x1, float y1, float x2, float y2, VornoiEdge e) {
        GraphEdge newEdge = new GraphEdge(e);
        allEdges.add(newEdge);

//        ReadVec2f p0 = new ReadVec2f(x1,y1);
//        ReadVec2f p1 = new ReadVec2f(x2,y2);
//        int p0index = pointIndices.get(p0);
//        if (p0index == -1) {
//            p0index = allPoints.size();
//            pointIndices.put(p0, p0index);
//            allPoints.add(p0);
//        }
//        int p1index = pointIndices.get(p1);
//        if (p1index == -1) {
//            p1index = allPoints.size();
//            pointIndices.put(p1, p1index);
//            allPoints.add(p1);
//        }

//        newEdge.p0index = p0index;
//        newEdge.p1index = p1index;
        newEdge.x1 = x1;
        newEdge.y1 = y1;
        newEdge.x2 = x2;
        newEdge.y2 = y2;
    }

    void line(float x1, float y1, float x2, float y2, VornoiEdge e) {
        pushGraphEdge(x1, y1, x2, y2, e);
    }

    void clip_line(VornoiEdge e) {
        float pxmin, pxmax, pymin, pymax;
        Site s1, s2;
        float x1 = 0, x2 = 0, y1 = 0, y2 = 0;

        x1 = e.reg0.x;
        x2 = e.reg1.x;
        y1 = e.reg0.y;
        y2 = e.reg1.y;

        // if the distance between the two points this line was created from is
        // less than the square root of 2, then ignore it
        if (Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1))) < minDistanceBetweenSites) {
            return;
        }
        pxmin = borderMinX;
        pxmax = borderMaxX;
        pymin = borderMinY;
        pymax = borderMaxY;

        if (e.a == 1.0 && e.b >= 0.0) {
            s1 = e.ep1;
            s2 = e.ep0;
        } else {
            s1 = e.ep0;
            s2 = e.ep1;
        }

        if (e.a == 1.0) {
            y1 = pymin;
            if (s1 != null && s1.y > pymin) {
                y1 = s1.y;
            }
            if (y1 > pymax) {
                y1 = pymax;
            }
            x1 = e.c - e.b * y1;
            y2 = pymax;
            if (s2 != null && s2.y < pymax)
                y2 = s2.y;

            if (y2 < pymin) {
                y2 = pymin;
            }
            x2 = (e.c) - (e.b) * y2;
            if (((x1 > pxmax) & (x2 > pxmax)) | ((x1 < pxmin) & (x2 < pxmin))) {
                return;
            }
            if (x1 > pxmax) {
                x1 = pxmax;
                y1 = (e.c - x1) / e.b;
            }
            if (x1 < pxmin) {
                x1 = pxmin;
                y1 = (e.c - x1) / e.b;
            }
            if (x2 > pxmax) {
                x2 = pxmax;
                y2 = (e.c - x2) / e.b;
            }
            if (x2 < pxmin) {
                x2 = pxmin;
                y2 = (e.c - x2) / e.b;
            }
        } else {
            x1 = pxmin;
            if (s1 != null && s1.x > pxmin)
                x1 = s1.x;
            if (x1 > pxmax) {
                x1 = pxmax;
            }
            y1 = e.c - e.a * x1;
            x2 = pxmax;
            if (s2 != null && s2.x < pxmax)
                x2 = s2.x;
            if (x2 < pxmin) {
                x2 = pxmin;
            }
            y2 = e.c - e.a * x2;
            if (((y1 > pymax) & (y2 > pymax)) | ((y1 < pymin) & (y2 < pymin))) {
                return;
            }
            if (y1 > pymax) {
                y1 = pymax;
                x1 = (e.c - y1) / e.a;
            }
            if (y1 < pymin) {
                y1 = pymin;
                x1 = (e.c - y1) / e.a;
            }
            if (y2 > pymax) {
                y2 = pymax;
                x2 = (e.c - y2) / e.a;
            }
            if (y2 < pymin) {
                y2 = pymin;
                x2 = (e.c - y2) / e.a;
            }
        }

        line(x1, y1, x2, y2, e);
    }

    void endpoint(VornoiEdge e, int lr, Site s) {
        if (lr == 0) {
            e.ep0 = s;
            if (e.ep1 == null) {
                return;
            }
        } else {
            e.ep1 = s;
            if (e.ep0 == null) {
                return;
            }
        }
        
        clip_line(e);
    }

    /* returns 1 if p is to right of halfedge e */
    boolean right_of(Halfedge el, float x, float y) {
        VornoiEdge e;
        Site topsite;
        boolean right_of_site;
        boolean above, fast;
        float dxp, dyp, dxs, t1, t2, t3, yl;

        e = el.ELedge;
        topsite = e.reg1;
        if (x > topsite.x)
            right_of_site = true;
        else
            right_of_site = false;
        if (right_of_site && el.ELpm == le)
            return (true);
        if (!right_of_site && el.ELpm == re)
            return (false);

        if (e.a == 1.0) {
            dyp = y - topsite.y;
            dxp = x - topsite.x;
            fast = false;
            if ((!right_of_site & (e.b < 0.0)) | (right_of_site & (e.b >= 0.0))) {
                above = dyp >= e.b * dxp;
                fast = above;
            } else {
                above = x + y * e.b > e.c;
                if (e.b < 0.0)
                    above = !above;
                if (!above)
                    fast = true;
            }
            if (!fast) {
                dxs = topsite.x - (e.reg0).x;
                above = e.b * (dxp * dxp - dyp * dyp) < dxs * dyp
                        * (1.0 + 2.0 * dxp / dxs + e.b * e.b);
                if (e.b < 0.0)
                    above = !above;
            }
        } else /* e.b==1.0 */
        {
            yl = e.c - e.a * x;
            t1 = y - yl;
            t2 = x - topsite.x;
            t3 = yl - topsite.y;
            above = t1 * t1 > t2 * t2 + t3 * t3;
        }
        return (el.ELpm == le ? above : !above);
    }

    Site rightreg(Halfedge he) {
        if (he.ELedge == (VornoiEdge) null)
            // if this halfedge has no edge, return the bottom site (whatever
            // that is)
            return (bottomsite);

        // if the ELpm field is zero, return the site 0 that this edge bisects,
        // otherwise return site number 1
        return (he.ELpm == le ? he.ELedge.reg1 : he.ELedge.reg0);
    }

    float dist(Site s, Site t) {
        float dx, dy;
        dx = s.x - t.x;
        dy = s.y - t.y;
        return (float) (Math.sqrt(dx * dx + dy * dy));
    }

    // create a new site where the HalfEdges el1 and el2 intersect - note that
    // the Point in the argument list is not used, don't know why it's there
    Site intersect(Halfedge el1, Halfedge el2) {
        VornoiEdge e1, e2, e;
        Halfedge el;
        float d, xint, yint;
        boolean right_of_site;
        Site v;

        e1 = el1.ELedge;
        e2 = el2.ELedge;
        if (e1 == null || e2 == null)
            return null;

        // if the two edges bisect the same parent, return null
        if (e1.reg1 == e2.reg1)
            return null;

        d = e1.a * e2.b - e1.b * e2.a;
        if (-1.0e-10 < d && d < 1.0e-10)
            return null;

        xint = (e1.c * e2.b - e2.c * e1.b) / d;
        yint = (e2.c * e1.a - e1.c * e2.a) / d;

        if ((e1.reg1.y < e2.reg1.y)
                || (e1.reg1.y == e2.reg1.y && e1.reg1.x < e2.reg1.x)) {
            el = el1;
            e = e1;
        } else {
            el = el2;
            e = e2;
        }

        right_of_site = xint >= e.reg1.x;
        if ((right_of_site && el.ELpm == le)
                || (!right_of_site && el.ELpm == re))
            return null;

        // create a new site at the point of intersection - this is a new vector
        // event waiting to happen
        v = new Site();
        v.x = xint;
        v.y = yint;
        return (v);
    }

    void out_triple(Site s1, Site s2, Site s3) {

    }

    int beachline_bake[] = null;

    void out_beachline() {

    }

    void out_site(Site s1) {
    }

	/*
	 * implicit parameters: nsites, sqrt_nsites, xmin, xmax, ymin, ymax, deltax,
	 * deltay (can all be estimates). Performance suffers if they are wrong;
	 * better to make nsites, deltax, and deltay too big than too small. (?)
	 */
    boolean voronoi_bd() {
        Site newsite, bot, top, temp, p;
        Site v;
        Point newintstar = null;
        Point tmpPoint = new Point();
        int pm;
        Halfedge lbnd, rbnd, llbnd, rrbnd, bisector;
        VornoiEdge e;

        PQinitialize();
        ELinitialize();

        bottomsite = nextone();
        out_site(bottomsite);
        newsite = nextone();
        while (true) {
            if (!PQempty())
                newintstar = PQ_min(tmpPoint);
            // if the lowest site has a smaller y value than the lowest vector
            // intersection,
            // process the site otherwise process the vector intersection

            if (newsite != null
                    && (PQempty() || newsite.y < newintstar.y || (newsite.y == newintstar.y && newsite.x < newintstar.x))) {
                out_site(newsite);
				/* new site is smallest -this is a site event */
                // get the first HalfEdge to the LEFT of the new site
                lbnd = ELleftbnd(newsite.x,newsite.y);
                // get the first HalfEdge to the RIGHT of the new site
                rbnd = ELright(lbnd);
                // if this halfedge has no edge,bot =bottom site (whatever that
                // is)
                bot = rightreg(lbnd);
                // create a new edge that bisects
                e = bisect(bot, newsite);

                // create a new HalfEdge, setting its ELpm field to 0
                bisector = HEcreate(e, le);
                // insert this new bisector edge between the left and right
                // vectors in a linked list
                ELinsert(lbnd, bisector);

                // if the new bisector intersects with the left edge,
                // remove the left edge's vertex, and put in the new one
                if ((p = intersect(lbnd, bisector)) != null) {
                    PQdelete(lbnd);
                    PQinsert(lbnd, p, dist(p, newsite));
                }
                lbnd = bisector;
                // create a new HalfEdge, setting its ELpm field to 1
                bisector = HEcreate(e, re);
                // insert the new HE to the right of the original bisector
                // earlier in the IF stmt
                ELinsert(lbnd, bisector);

                // if this new bisector intersects with the new HalfEdge
                if ((p = intersect(bisector, rbnd)) != null) {
                    // push the HE into the ordered linked list of vertices
                    PQinsert(bisector, p, dist(p, newsite));
                }
                out_beachline();
                newsite = nextone();
            } else if (!PQempty())
			/* intersection is smallest - this is a vector event */
            {
                // pop the HalfEdge with the lowest vector off the ordered list
                // of vectors
                lbnd = PQextractmin();
                // get the HalfEdge to the left of the above HE
                llbnd = ELleft(lbnd);
                // get the HalfEdge to the right of the above HE
                rbnd = ELright(lbnd);
                // get the HalfEdge to the right of the HE to the right of the
                // lowest HE
                rrbnd = ELright(rbnd);
                // get the Site to the left of the left HE which it bisects
                bot = leftreg(lbnd);
                // get the Site to the right of the right HE which it bisects
                top = rightreg(rbnd);

                // output the triple of sites, stating that a circle goes
                // through them
                out_triple(bot, top, rightreg(lbnd));

                v = lbnd.vertex; // get the vertex that caused this event
                makevertex(v); // set the vertex number - couldn't do this
                // earlier since we didn't know when it would be processed
                endpoint(lbnd.ELedge, lbnd.ELpm, v);
                // set the endpoint of
                // the left HalfEdge to be this vector
                endpoint(rbnd.ELedge, rbnd.ELpm, v);
                // set the endpoint of the right HalfEdge to
                // be this vector
                ELdelete(lbnd); // mark the lowest HE for
                // deletion - can't delete yet because there might be pointers
                // to it in Hash Map
                PQdelete(rbnd);
                // remove all vertex events to do with the right HE
                ELdelete(rbnd); // mark the right HE for
                // deletion - can't delete yet because there might be pointers
                // to it in Hash Map
                pm = le; // set the pm variable to zero

                if (bot.y > top.y)
                // if the site to the left of the event is higher than the
                // Site
                { // to the right of it, then swap them and set the 'pm'
                    // variable to 1
                    temp = bot;
                    bot = top;
                    top = temp;
                    pm = re;
                }
                e = bisect(bot, top); // create an Edge (or line)
                // that is between the two Sites. This creates the formula of
                // the line, and assigns a line number to it
                bisector = HEcreate(e, pm); // create a HE from the Edge 'e',
                // and make it point to that edge
                // with its ELedge field
                ELinsert(llbnd, bisector); // insert the new bisector to the
                // right of the left HE
                endpoint(e, re - pm, v); // set one endpoint to the new edge
                // to be the vector point 'v'.
                // If the site to the left of this bisector is higher than the
                // right Site, then this endpoint
                // is put in position 0; otherwise in pos 1

                // if left HE and the new bisector intersect, then delete
                // the left HE, and reinsert it
                if ((p = intersect(llbnd, bisector)) != null) {
                    PQdelete(llbnd);
                    PQinsert(llbnd, p, dist(p, bot));
                }

                // if right HE and the new bisector intersect, then
                // reinsert it
                if ((p = intersect(bisector, rrbnd)) != null) {
                    PQinsert(bisector, p, dist(p, bot));
                }
            } else
                break;
        }

        for (lbnd = ELright(ELleftend); lbnd != ELrightend; lbnd = ELright(lbnd)) {
            e = lbnd.ELedge;
            clip_line(e);
        }
        endsim();
        return true;
    }

    public boolean generateVoronoi(float minX, float maxX, float minY,
                                   float maxY) {
        float temp = 0;
        if (minX > maxX) {
            temp = minX;
            minX = maxX;
            maxX = temp;
        }
        if (minY > maxY) {
            temp = minY;
            minY = maxY;
            maxY = temp;
        }
        borderMinX = minX;
        borderMinY = minY;
        borderMaxX = maxX;
        borderMaxY = maxY;

        siteidx = 0;
        voronoi_bd();

        return true;
    }
    void endsim()
    {

    }
}

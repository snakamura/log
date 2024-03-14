# Getting a frame from bounds

When you apply a transformation to `UIView`, its frame property becomes undefined and you need to calculate its frame by yourself. If you apply a transformation more than just a scaling or a translation, it's convenient to get its frame as a path.

This method returns `UIBezierPath` of its frame.

    - (UIBezierPath *)framePath {
        UIBezierPath *path = [UIBezierPath bezierPathWithRect:CGRectOffset(self.bounds, -self.bounds.size.width/2, -self.bounds.size.height/2)];
        [path applyTransform:self.transform];
        [path applyTransform:CGAffineTransformMakeTranslation(self.center.x, self.center.y)];
        return path;
    }


-- stackBox :: (ManagedPtr widget -> widget ) -> StackBoxChild -> Widget event
-- 
-- StackboxChild 
--
--
--

{-data StackBox widget children event where
    StackBox :: 
        (Gtk.ManagedPtr widget -> widget)
        -> Vector (Attribute widget event)
        -> StackBoxChild event
        -> StackBox widget children event

data StackBoxChild event = StackBoxChild
    { theStack :: Widget event
    , theSidebar :: Widget event
    }

instance Patchable (Stackbox widget StackBoxChild)-}

stackBox :: Vector (Attribute widget event) -> 

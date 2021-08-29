module Metal.Messages.FileInfo where
data FileInfo = FileInfo {
  -- | @size k@ is the byte-based length of the file which @k@
  -- describes.
  size :: Maybe Integer,
  -- | @mimetype k@ is the MIME type of the file which @k@ describes.
  mimetype :: Maybe String
} deriving (Eq, Read, Show);

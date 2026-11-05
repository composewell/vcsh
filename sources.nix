{nixpack}:
# Package sources to override the streamly-packages set.
with nixpack.mkSources;
let
  cwgh = repo: rev: gh "composewell" repo rev;
in
{
layers = [
{
  streamly-core = hackage "0.3.2"
    "1bhxdddmpg6x27kcsg9qrdsz2ggqmirlrgd7l8v6kb5xq60lf2sb";
  streamly = hackage "0.11.1"
    "1dxyhq8m9fr3ghpmxkh6bc37fd4f1048xckjrlpp6ybvlg0lq7g2"
    // { rev = "2";
         cabalSha256 = "022hh6aw4sa6a5z6anmngrb4lgh9xql824jn68m01pppqszd9syz";
       };
  streamly-coreutils = cwgh "streamly-coreutils"
    "809565fa870dcb1d1273f982c06a88cfc9e69237";
  vcsh = local ./.;
}
];
}

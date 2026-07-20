#include <cstdint>
#include <cstdio>
#include <string>
#include <cpp4r.hpp>

using namespace cpp4r;

/* roxygen
@title Hash Raw Bytes (C++)
@description Computes a fast, well-mixed 128-bit (32 hex character) hash of
  a raw vector. Used internally by `.tabler_cache_key()` to turn the raw
  bytes from `serialize(x, connection = NULL)` into a cachem-compatible
  cache key, without depending on `rlang`/`digest` or touching disk (unlike
  `tools::md5sum()`, which only hashes files).
@param x A raw vector, e.g. the output of `serialize()`.
@return A 32-character lowercase hex string.
@keywords internal
@noRd
*/
[[cpp4r::register]]
std::string tabler_hash_raw(raws x) {
  uint64_t h1 = 0xcbf29ce484222325ULL;             // FNV-1a 64-bit offset basis
  uint64_t h2 = 0x9e3779b97f4a7c15ULL;             // 2nd lane seed (golden ratio)
  const uint64_t prime1 = 0x100000001b3ULL;        // FNV-1a 64-bit prime
  const uint64_t prime2 = 0xc2b2ae3d27d4eb4fULL;   // independent odd prime

  for (uint8_t byte : x) {
    h1 = (h1 ^ byte) * prime1;
    h2 = (h2 ^ byte) * prime2;
  }

  char buf[33];
  std::snprintf(buf, sizeof(buf), "%016llx%016llx",
                (unsigned long long)h1, (unsigned long long)h2);
  return std::string(buf, 32);
}

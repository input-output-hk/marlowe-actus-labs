import browserOrNode from 'browser-or-node';

export function fromBytesImpl(lib, bytes) {
  return lib.Address.from_bytes(bytes);
}

export function toBech32Impl(address) {
  return address.to_bech32();
}

export function toJsonImpl(address) {
  return address.to_json();
}

export function freeImpl(address) {
  address.free();
}

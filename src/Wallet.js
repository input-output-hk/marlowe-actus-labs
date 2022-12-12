export const getWallets_ = w => () => w.cardano
  ? Object.fromEntries(Object.entries(w.cardano).filter(([,value]) => value !== null && typeof value === "object" && typeof value.enable === "function"))
  : null

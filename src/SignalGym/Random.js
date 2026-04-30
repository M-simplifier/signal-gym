export const randomSeed = () => {
  const max = 2147483647;

  try {
    if (globalThis.crypto && typeof globalThis.crypto.getRandomValues === "function") {
      const values = new Uint32Array(1);
      globalThis.crypto.getRandomValues(values);
      return (values[0] % max) + 1;
    }
  } catch {
    // Fall through to Math.random.
  }

  return Math.floor(Math.random() * max) + 1;
};

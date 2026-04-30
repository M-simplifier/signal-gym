const key = "signal-gym.profile.v1";

const pad = value => String(value).padStart(2, "0");

const dayKey = offset => {
  const date = new Date();
  date.setDate(date.getDate() + offset);
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}`;
};

const defaults = () => ({
  xp: 0,
  streak: 0,
  lastDay: "",
  todayKey: dayKey(0),
  yesterdayKey: dayKey(-1),
  sessions: 0,
  bestScore: 0,
  focusMinutes: 0,
  gateLevel: 2,
  traceLevel: 2,
  readLevel: 2
});

const cleanInt = (value, fallback) => {
  const parsed = Number.parseInt(value, 10);
  return Number.isFinite(parsed) ? parsed : fallback;
};

const normalize = raw => {
  const base = defaults();
  const value = raw && typeof raw === "object" ? raw : {};

  return {
    xp: cleanInt(value.xp, base.xp),
    streak: cleanInt(value.streak, base.streak),
    lastDay: typeof value.lastDay === "string" ? value.lastDay : base.lastDay,
    todayKey: base.todayKey,
    yesterdayKey: base.yesterdayKey,
    sessions: cleanInt(value.sessions, base.sessions),
    bestScore: cleanInt(value.bestScore, base.bestScore),
    focusMinutes: cleanInt(value.focusMinutes, base.focusMinutes),
    gateLevel: Math.max(1, Math.min(9, cleanInt(value.gateLevel, base.gateLevel))),
    traceLevel: Math.max(1, Math.min(9, cleanInt(value.traceLevel, base.traceLevel))),
    readLevel: Math.max(1, Math.min(9, cleanInt(value.readLevel, base.readLevel)))
  };
};

export const loadProfile = () => {
  try {
    const stored = window.localStorage.getItem(key);
    return normalize(stored ? JSON.parse(stored) : null);
  } catch {
    return defaults();
  }
};

export const saveProfile = profile => () => {
  const payload = normalize(profile);
  window.localStorage.setItem(key, JSON.stringify({
    xp: payload.xp,
    streak: payload.streak,
    lastDay: payload.lastDay,
    sessions: payload.sessions,
    bestScore: payload.bestScore,
    focusMinutes: payload.focusMinutes,
    gateLevel: payload.gateLevel,
    traceLevel: payload.traceLevel,
    readLevel: payload.readLevel
  }));
};

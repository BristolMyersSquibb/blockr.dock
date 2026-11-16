// What the dock makes of something thrown, which may be anything: its message,
// and an Error that the dock's log takes.

// The message of an error, or of anything else thrown, as a string.
export function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

// The error, or an Error with the message of anything else thrown.
export function asError(error: unknown): Error {
  return error instanceof Error ? error : new Error(errorMessage(error));
}

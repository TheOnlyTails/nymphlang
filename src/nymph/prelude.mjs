/**
 * @template {string[]} const A
 * @template {Record<A[number], any>} P
 * @template R
 */
export class Func {
	/**
	 * @type {(params: P) => R}
	 */
	func;

	/**
	 * @param {A} params
	 * @param {(params: P) => R} func
	 */
	constructor(params, func) {
		this.params = params;
		this.func = func;
	}

	/**
	 * @param {Array<P[string]>} positionalArgs
	 * @param {Partial<P>} namedArgs
	 * @returns {R}
	 */
	call(positionalArgs, namedArgs) {
		/**
		 * @type {P}
		 */
		const args = {};
		for (let name in this.params) {
			if (name in namedArgs) args[name] = namedArgs[name];
			else args[name] = positionalArgs.shift();
		}
		return this.func.call(args);
	}
}

/**
 * @type {unique symbol}
 */
export const RangeFull = new Symbol("nymph/full_range");

/**
 * @extends {IterableIterator<number>}
 */
export class RangeExclusive {
	/**
	 * @type {number}
	 */
	min;
	/**
	 * @type {number}
	 */
	max;

	/**
	 * @param {number} min
	 * @param {number} max
	 */
	constructor(min, max) {
		this.min = min;
		this.max = max;
	}

	*[Symbol.iterator]() {
		for (let i = min; i < max; i++) yield { value: i };
		yield { done: true };
	}
}

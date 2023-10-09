<script>
    import { onDestroy } from 'svelte';
    import Stopwatch from './Stopwatch.svelte';
    import Laps from './Laps.svelte';
    import Controls from './Controls.svelte';
    import Selector from './Selector.svelte';

	// import the number of milliseconds from the readable stores
	import { time } from './stores.js';

    // lapse is set to consider the time since the start button is first pressed
    let lapse = 0;

    let previous = 0;

    // paused is set to record the time accumulated before the pause button is pressed
    let paused = 0;

    // unsubscribe is set to refer to the function used to unsubscribe from the store
    let unsubscribe;

    // through the start function pair the lapse variable to the time retrieved from the readable store
    function start() {
        // assign the stop function to unsubscribe
        unsubscribe = time.subscribe(value => {
            // add the previous value to the current number of milliseconds
            //lapse = value + previous;
            lapse = value + paused;
        });
    }

    // through the terminate function unsubscribe from the readable store
    function terminate() {
        // check if unsubscribe is truthy (this to cover the situation in which the stop button is pressed after the pause button)
        if (unsubscribe) {
            unsubscribe();
            unsubscribe = null;
        }
    }

    // through the stop function unsubscribe from the readable store and reset the values
    function stop() {
        if (!confirm(`Voulez-vous vraiment supprimer ${selected.name} ?`)) {
          return
        }
        stopwatches = stopwatches.filter(elem => elem.name != selected.name);
        if (stopwatches.length > 0) {
        	selected = stopwatches[0];
	} else {
		selected = null;
	}
        terminate();
    }

    // through the pause function unsubscribe from the store and set previous to match the value held by lapse
    function pause() {
        paused = lapse;
        terminate();
    }

    let stopwatches = [];
    let selected = null;
    function add() {
       let name = prompt("Votre chronomètre", "Sans nom");
       let sw = { name: name, laps: [], lapse: 0, previous: 0, paused: 0 };
       stopwatches = [ sw, ...stopwatches ];
       switch_stopwatch(sw);
    }

    function sel(event) {
     	switch_stopwatch(event.detail);
    }

    function switch_stopwatch(sw) {
        if (selected) {
          let old = stopwatches.find(elem => elem.name === selected.name);
          old.lapse = lapse;
          old.previous = previous;
          old.laps = laps;
          old.paused = paused;
        }
        selected = stopwatches.find(elem => elem.name === sw.name);
        lapse = selected.lapse;
        previous = selected.previous;
        laps = selected.laps;
        paused = selected.paused;
    }

    // describe the booleans to determine the button(s) included in the controls component
    // subscription refers to the state in which the start button has been pressed
    // here the subscription is ongoing and the unsubscribe variable holds a truthy value
    $:subscription = !!unsubscribe;
    // lapsed refers to the state in which the subscription  has started and lapse holds a value greater than 0
    $:lapsed = !!lapse;

    // laps refers to an array describing the number of milliseconds after each lap
    let laps = [];

    // through the lap function include an object specifying the total and partial number of milliseconds
    function lap() {
        const { length } = laps;
        previous += lapse;
        lapse = 0;
        const total = previous;
        // partial referring to the number of milliseconds between the previous (if existing) and current lap
        const partial = length > 0 ? total - laps[0].total : total;
        laps = [{
            number: length + 1,
            total,
            partial,
        }, ...laps];
        paused = 0;
	terminate();
    }

    // unsubscribe from the store to avoid memory leaks
    onDestroy(() => {
        terminate();
    });

</script>
<style>
	/* global styles which would otherwise be placed in the global stylesheet */

	:global(*) {
			box-sizing: border-box;
			padding: 0;
			margin: 0;
        		font-family: monospace;
	}
	:global(h1) {
        	font-family: monospace;
		font-weight: normal;
	}

	:global(body) {
			background: hsl(0, 0%, 95%);
			color: hsl(0, 0%, 20%);
        		font-family: monospace;
			/* center the .stopwatch container in the viewport */
			display: flex;
			flex-direction: column;
			justify-content: center;
			align-items: center;
			min-height: 100vh;
	}
	
	/* display the content of the .stopwatch container in a column */
	:global(.stopwatch) {
			display: flex;
			flex-direction: column;
	}
	:global(.stopwatch > * + *), :global(.selector) {
			margin-top: 0.75rem;
	}
	
	/* for devices supporting css grid */
	@supports (display: grid) {
			/* for larger viewports */
			@media (min-width: 700px) {
					/* display the svg, ul and .controls in a grid

				|   svg  |  ul           |
				|   svg  |   .controls   |
			*/
					:global(.stopwatch) {
							display: grid;
							grid-gap: 20px 50px;
							grid-template-columns: 300px 350px;
							grid-template-rows: 225px auto;
							grid-template-areas: "watch list" "watch controls";
							justify-content: space-between;
					}
					:global(.stopwatch svg) {
							grid-area: watch;
							align-self: center;
					}
					:global(.stopwatch ul) {
							grid-area: list;
					}
					:global(.stopwatch .controls) {
							grid-area: controls;
							align-self: center;
					}
					:global(.stopwatch > * + *) {
							margin-top: 0;
					}
			}
	}

</style>

{#if selected}
<h1>{selected.name}</h1>
<div class="stopwatch">
    <!-- pass the number of milliseconds to the stopwatch component -->
    <Stopwatch {lapse} />
    <!-- pass the array of laps to the laps component -->
    <Laps {laps} />
    <!-- following the events disaptched from the controls component call the start/pause/stop/lap function
    pass the necessary booleans to display the correct button(s)
    -->
	<Controls on:start={start} on:stop={stop} on:pause={pause} on:lap={lap} {subscription} {lapsed} />
</div>
{/if}

<Selector on:add={add} on:sel={sel} {stopwatches} {selected} {subscription} />

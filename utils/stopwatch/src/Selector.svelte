<script>
    import { createEventDispatcher } from "svelte";
    const dispatch = createEventDispatcher();

    function add() {
       dispatch("add");
    }

    function sel(sw) {
       console.log(sw);
       dispatch("sel", sw);
    }

    export let stopwatches = [ ];
    export let selected = null;
</script>

<style>
    .swcont {
       width: 90%;
       overflow-x: scroll;
    }

    .swlist {
        margin-top: 1.5rem;
        margin-bottom: 0.5rem;
        min-width: 100%;
        display: inline-flex;
        border: 1px solid hsl(0, 0%, 25%);
        border-radius: 20px;
    }

    .swlist button {
        font-size: 0.9rem;
        color: hsl(0, 0%, 10%);
        font-family: inherit;
        padding: 0.5rem;
        flex-grow: 1;
        min-width: 6rem;
        border: none;
        background: none;
        border-radius: inherit;
        margin: 0; /* by default svelte applies a margin to the bottom of the button */
        transition: background 0.2s ease-out;
    }

    .swlist button:not(:first-of-type) {
        border-left: 1px solid hsl(0, 0%, 25%);
        border-top-left-radius: 0;
        border-bottom-left-radius: 0;
    }

    .swlist button:focus {
        outline-color: hsl(0, 0%, 25%);
    }
    .swlist button:hover {
        background: hsl(0, 0%, 92%);
    }
    .swlist button.selected {
        background: hsl(0, 0%, 85%);
    }
</style>

<div class="swcont">
<div class="swlist">
    <button style="min-width: 3rem; width: 3rem" on:click="{add}">+</button>
    {#each stopwatches as sw}
    {#if selected && selected.name === sw.name}
    <button on:click="{sel}" class="selected">{sw.name}</button>
    {:else}
    <button on:click="{() => sel(sw)}">{sw.name}</button>
    {/if}
    {/each}
</div>
</div>

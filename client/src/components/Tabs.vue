<script>
import { provide, computed, ref } from "vue";

export default {
    name: "Tabs",
    props: {
        modelValue: {
            type: [String, Number],
        },
    },
    emits: ["update:modelValue"],
    setup(props, { emit }) {
        const active = computed(() => props.modelValue);
        const tabs = ref([]);

        function selectTab(tab) {
            emit("update:modelValue", tab);
        }
        provide("tabsState", {
            active,
            tabs,
        });
        return {
            tabs,
            active,
            selectTab,
        };
    },
};
</script>

<template>
    <div class="mx-80 object-center">
        <ul class="flex flex-row">
            <li
                v-for="(tab, i) of tabs"
                :key="i"
                :class="
                    active === i
                        ? 'border-b-4 border-yellow-500 text-gray-800'
                        : 'border-b-2 border-white text-gray-500'
                "
                class="flex items-center px-6 py-3 rounded-tl-md rounded-tr-md overflow-hidden cursor-pointer hover:text-gray-800"
                @click="selectTab(i)"
            >
                {{ tab.props.title }}
            </li>
        </ul>
        <div class="bg-gray-300 -m-1 h-1"></div>
        <div class="mt-6">
            <slot />
        </div>
    </div>
</template>

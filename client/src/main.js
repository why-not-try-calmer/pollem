import { createApp } from 'vue'
import App from './App.vue'

import Toaster from '@meforma/vue-toaster';
import Visualizer from './components/Visualizer.vue';

createApp(App).use(Toaster, { position: "bottom" }).mount('#app')

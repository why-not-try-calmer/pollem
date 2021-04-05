import { createApp } from 'vue'
import App from './App.vue'

import Toaster from '@meforma/vue-toaster';

createApp(App).use(Toaster, { position: "bottom", maxToasts : 1 }).mount('#app')

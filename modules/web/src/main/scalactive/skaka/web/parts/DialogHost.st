<script lang="scala">
import skaka.web.*

// 画面内の確認のダイアログ. Dialog.confirm で開く.
def okClass(d: DialogReq): String = if (d.danger) "danger-fill" else "primary"
def list = Dialog.current.mapC(_.toList)
</script>

<template>
  <Modal v-for="d in list" :onClose="_ => Dialog.close(false)">
    <form class="dlg" @submit.prevent="Dialog.close(true)">
      <p class="msg">{{ d.message }}</p>
      <div class="actions">
        <button type="button" @click="Dialog.close(false)">キャンセル</button>
        <button type="submit" :class="okClass(d)" autofocus>{{ d.okLabel }}</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.msg { white-space: pre-wrap; margin: 0 0 .8rem; line-height: 1.6; font-weight: 600; }
.danger-fill { background: var(--danger); border-color: var(--danger); color: #fff; }
</style>

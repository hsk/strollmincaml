#include <iostream>
#include <vector>
#include <algorithm>

struct FrameMap {
  int32_t NumRoots; // ルート数
  int32_t NumMeta; // メタデータ数
  void *Meta[]; // メタデータの中身
};

// スタックエントリ
struct StackEntry {
  StackEntry *Next; // 次のスタックエントリ
  FrameMap *Map; // フレームマップ
  void *Roots[]; // ルート配列
};

// スタックエントリ
StackEntry *llvm_gc_root_chain;

// ルート検索
template<typename F>
void visitGCRoots(F const& Visitor) {
  for (StackEntry *entry = llvm_gc_root_chain; entry != nullptr; entry = entry->Next) {
    unsigned i = 0;

    for (; i < entry->Map->NumMeta; ++i)
      Visitor(&entry->Roots[i], entry->Map->Meta[i]);

    for (; i < entry->Map->NumRoots; ++i)
      Visitor(&entry->Roots[i], nullptr);
  }
}

// メモリ管理用のデータ
struct element {
  void* pointer; // ポインタを保存
  bool marked; // マーク用
};


static class GC {
// メモリアロケーション
public:
  void* alloc(int32_t size) {

    // サイズが10以上でGCが動く
    if (elements_.size() >= 10) {
      std::cout << "-- Triggered Garbage Collector!!" << std::endl;
      all_collect();
    }

    // メモリ確保
    void* const p = malloc(size);

    // 要素リストに保存
    element e{p, false};
    elements_.push_back(e);

    return p;
  }

// GCを実行する関数
public:
  void all_collect() {

    // 全マークフラグを下ろす
    for (auto& elm : elements_)
      elm.marked = false;

    // マークフェーズ
    std::cout << "!! Start to visit ShadowStack" << std::endl;
    visitGCRoots([&](void **Root, const void *Meta) {
      // Rootは生きている
      std::cout << "-> Alived pointer: " << "(" << *Root << ")" << std::endl;
      // Rootを要素リストから検索
      auto const it = find_if(elements_.begin(), elements_.end(), [&](element const& elm ) {
        return elm.pointer == *Root;
      });
      // 見つけた場合マークする
      if (it != elements_.end())
        it->marked = true;
    } );

    // 状態表示
    show_object_status();

    // スイープフェーズ
    for (auto& elm : elements_) {
      // マークされていない時
      if (elm.marked == false) {
        // メモリを解放
        free(elm.pointer);
        std::cout << "! Destructed( Addr: " << elm.pointer << " )" << std::endl;
      }
    }
    // 削除する要素リストを作成
    auto const it = remove_if(elements_.begin(), elements_.end(), [](element const& elm) {
      return elm.marked == false;
    });
    // 削除要素を削除
    elements_.erase(it, elements_.end());

    // 状態表示
    std::cout << "!! Garbege Collected" << std::endl;
    show_object_status();

    std::cout << "-- Finished" << std::endl;
  }

// 状態表示
private:
  void show_object_status() const {
    std::cout << "- Object Status" << std::endl;
    // 要素をループして表示
    for (auto const& elm : elements_) {
      std::cout << "? Addr: " << elm.pointer << "(" << ( elm.marked ? "Alive" : "Dead" ) << ")" << std::endl;
    }
  }

// 要素リスト
private:
  std::vector<element> elements_;
} gcobj;


// Cのインターフェイス
extern "C" {
  // アロケーション
  void* my_alloc(int32_t size) {
    return gcobj.alloc(size);
  }
  void gc() {
    gcobj.all_collect();
  }
}

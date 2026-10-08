// Bookmark id migration. Run with: node tests/js/test-bookmark-ids.js
// (also run by tests/testthat/test-app-bookmarks.R when Node is installed)
const assert = require('assert');
const path = require('path');
const ids = require(path.join(__dirname, '..', '..', 'www', 'bookmark_ids.js'));

// base ids
assert.strictEqual(ids.baseId('2403.01234v2'), '2403.01234');
assert.strictEqual(ids.baseId('2403.01234v12'), '2403.01234');
assert.strictEqual(ids.baseId('2403.01234'), '2403.01234');
assert.strictEqual(ids.baseId('math/0406049v1'), 'math/0406049');
assert.strictEqual(ids.baseId(' 2403.01234v3 '), '2403.01234');

// version 3 storage with versioned ids -> base ids, order kept, duplicates merged
let out = ids.migrateStorage({ bookmarks: ['2403.01234v2', '2501.00001v1', '2403.01234v3', 'math/0406049v1'], version: 3 });
assert.deepStrictEqual(out.data.bookmarks, ['2403.01234', '2501.00001', 'math/0406049']);
assert.strictEqual(out.data.version, ids.STORAGE_VERSION);
assert.strictEqual(out.changed, true);

// migrating twice changes nothing: the migration runs once
const again = ids.migrateStorage(out.data);
assert.deepStrictEqual(again.data, out.data);
assert.strictEqual(again.changed, false);

// already-base ids under the old version number: only the version changes
out = ids.migrateStorage({ bookmarks: ['2403.01234'], version: 3 });
assert.deepStrictEqual(out.data.bookmarks, ['2403.01234']);
assert.strictEqual(out.changed, true);

// damaged or empty storage
assert.deepStrictEqual(ids.migrateStorage(null).data.bookmarks, []);
assert.deepStrictEqual(ids.migrateStorage({}).data.bookmarks, []);
assert.deepStrictEqual(ids.migrateStorage({ bookmarks: 'oops' }).data.bookmarks, []);
assert.deepStrictEqual(ids.migrateStorage({ bookmarks: [null, '', '2403.01234v1'], version: 3 }).data.bookmarks, ['2403.01234']);

console.log('bookmark id tests passed');

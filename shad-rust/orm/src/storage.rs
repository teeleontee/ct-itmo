use crate::{
    data::{DataType, Value},
    error::Result,
    object::Schema,
    Error, ObjectId,
};

use rusqlite::{OptionalExtension, NO_PARAMS};

use std::{borrow::Cow, error::Error, fmt::Write};

////////////////////////////////////////////////////////////////////////////////

pub type Row<'a> = Vec<Value<'a>>;
pub type RowSlice<'a> = [Value<'a>];

////////////////////////////////////////////////////////////////////////////////

pub(crate) trait StorageTransaction {
    fn table_exists(&self, table: &str) -> Result<bool>;
    fn create_table(&self, schema: &Schema) -> Result<()>;

    fn insert_row(&self, schema: &Schema, row: &RowSlice) -> Result<ObjectId>;
    fn update_row(&self, id: ObjectId, schema: &Schema, row: &RowSlice) -> Result<()>;
    fn select_row(&self, id: ObjectId, schema: &Schema) -> Result<Row<'static>>;
    fn delete_row(&self, id: ObjectId, schema: &Schema) -> Result<()>;

    fn commit(&self) -> Result<()>;
    fn rollback(&self) -> Result<()>;
}

impl<'a> StorageTransaction for rusqlite::Transaction<'a> {
    fn table_exists(&self, table: &str) -> Result<bool> {
        let query = format!(
            "SELECT count(*) FROM sqlite_master WHERE type='table' AND name={}",
            table
        );
        let mut stmt = self.prepare(&query)?;
        let rows_iter = stmt.query_map([], |row| {
            let tmp: i32 = row.get(0)?;
            Ok(tmp)
        })?;
        for item in rows_iter {
            if let Ok(val) = item {
                match val {
                    0 => Result::Ok(false),
                    1 => Result::Ok(true),
                };
            }
        }

        Result::Ok(false)
    }

    fn create_table(&self, schema: &Schema) -> Result<()> {
        Ok(())
    }

    fn insert_row(&self, schema: &Schema, row: &RowSlice) -> Result<ObjectId> {
        // TODO: your code here.
        unimplemented!()
    }

    fn update_row(&self, id: ObjectId, schema: &Schema, row: &RowSlice) -> Result<()> {
        // TODO: your code here.
        unimplemented!()
    }

    fn select_row(&self, id: ObjectId, schema: &Schema) -> Result<Row<'static>> {
        // TODO: your code here.
        unimplemented!()
    }

    fn delete_row(&self, id: ObjectId, schema: &Schema) -> Result<()> {
        // TODO: your code here.
        unimplemented!()
    }

    fn commit(&self) -> Result<()> {
        // TODO: your code here.
        unimplemented!()
    }

    fn rollback(&self) -> Result<()> {
        // TODO: your code here.
        unimplemented!()
    }
}

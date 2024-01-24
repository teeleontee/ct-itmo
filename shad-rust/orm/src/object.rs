use crate::{
    data::DataType,
    storage::{Row, RowSlice},
};

use std::any::Any;

////////////////////////////////////////////////////////////////////////////////

pub trait Object: Any {
    const scheme: Schema;
    fn to_row(&self) -> Row;
    fn from_row(row_slice: &RowSlice) -> Self;
}

////////////////////////////////////////////////////////////////////////////////

pub struct Schema {
    object_type: String,
    table_name: String,
    fields: &'static [Field]
}

pub struct Field {
    name: String,
    column_name: String,
    type_name: String,
}


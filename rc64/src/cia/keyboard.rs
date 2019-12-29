use enum_map::{enum_map, Enum, EnumMap};
use lazy_static::lazy_static;
use std::fmt;
use std::ops::Index;
use std::ops::IndexMut;

#[derive(Enum)]
pub enum C64Key {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,

    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,

    AtSign,
    Colon,
    Comma,
    Currency,
    Dash,
    Equal,
    Period,
    Plus,
    Semicolon,
    Slash,
    Space,
    Star,

    Ctrl,
    Delete,
    Home,
    LShift,
    RShift,
    Return,

    Commodore,
    Stop,

    Up,
    CursorDown,
    CursorRight,
    Left,

    F1,
    F3,
    F5,
    F7,
}

pub(super) type Row = u8;
pub(super) type Column = u8;

#[derive(Debug, Clone, Copy)]
pub(super) struct MatrixIndex {
    row: Row,
    column: Column,
}

impl MatrixIndex {
    pub fn rc(row: Row, column: Column) -> Self {
        MatrixIndex { row, column }
    }
}

use C64Key::*;
lazy_static! {
    static ref VIDEO_MATRIX_POS: EnumMap<C64Key, MatrixIndex> = enum_map! {
        Stop        => MatrixIndex { row: 7, column: 7 },
        Q           => MatrixIndex { row: 6, column: 7 },
        Commodore   => MatrixIndex { row: 5, column: 7 },
        Space       => MatrixIndex { row: 4, column: 7 },
        Two         => MatrixIndex { row: 3, column: 7 },
        Ctrl        => MatrixIndex { row: 2, column: 7 },
        Left        => MatrixIndex { row: 1, column: 7 },
        One         => MatrixIndex { row: 0, column: 7 },

        Slash       => MatrixIndex { row: 7, column: 6 },
        Up          => MatrixIndex { row: 6, column: 6 },
        Equal       => MatrixIndex { row: 5, column: 6 },
        RShift      => MatrixIndex { row: 4, column: 6 },
        Home        => MatrixIndex { row: 3, column: 6 },
        Semicolon   => MatrixIndex { row: 2, column: 6 },
        Star        => MatrixIndex { row: 1, column: 6 },
        Currency    => MatrixIndex { row: 0, column: 6 },

        Comma       => MatrixIndex { row: 7, column: 5 },
        AtSign      => MatrixIndex { row: 6, column: 5 },
        Colon       => MatrixIndex { row: 5, column: 5 },
        Period      => MatrixIndex { row: 4, column: 5 },
        Dash        => MatrixIndex { row: 3, column: 5 },
        L           => MatrixIndex { row: 2, column: 5 },
        P           => MatrixIndex { row: 1, column: 5 },
        Plus        => MatrixIndex { row: 0, column: 5 },

        N           => MatrixIndex { row: 7, column: 4 },
        O           => MatrixIndex { row: 6, column: 4 },
        K           => MatrixIndex { row: 5, column: 4 },
        M           => MatrixIndex { row: 4, column: 4 },
        Zero        => MatrixIndex { row: 3, column: 4 },
        J           => MatrixIndex { row: 2, column: 4 },
        I           => MatrixIndex { row: 1, column: 4 },
        Nine        => MatrixIndex { row: 0, column: 4 },

        V           => MatrixIndex { row: 7, column: 3 },
        U           => MatrixIndex { row: 6, column: 3 },
        H           => MatrixIndex { row: 5, column: 3 },
        B           => MatrixIndex { row: 4, column: 3 },
        Eight       => MatrixIndex { row: 3, column: 3 },
        G           => MatrixIndex { row: 2, column: 3 },
        Y           => MatrixIndex { row: 1, column: 3 },
        Seven       => MatrixIndex { row: 0, column: 3 },

        X           => MatrixIndex { row: 7, column: 2 },
        T           => MatrixIndex { row: 6, column: 2 },
        F           => MatrixIndex { row: 5, column: 2 },
        C           => MatrixIndex { row: 4, column: 2 },
        Six         => MatrixIndex { row: 3, column: 2 },
        D           => MatrixIndex { row: 2, column: 2 },
        R           => MatrixIndex { row: 1, column: 2 },
        Five        => MatrixIndex { row: 0, column: 2 },

        LShift      => MatrixIndex { row: 7, column: 1 },
        E           => MatrixIndex { row: 6, column: 1 },
        S           => MatrixIndex { row: 5, column: 1 },
        Z           => MatrixIndex { row: 4, column: 1 },
        Four        => MatrixIndex { row: 3, column: 1 },
        A           => MatrixIndex { row: 2, column: 1 },
        W           => MatrixIndex { row: 1, column: 1 },
        Three       => MatrixIndex { row: 0, column: 1 },

        CursorDown  => MatrixIndex { row: 7, column: 0 },
        F5          => MatrixIndex { row: 6, column: 0 },
        F3          => MatrixIndex { row: 5, column: 0 },
        F1          => MatrixIndex { row: 4, column: 0 },
        F7          => MatrixIndex { row: 3, column: 0 },
        CursorRight => MatrixIndex { row: 2, column: 0 },
        Return      => MatrixIndex { row: 1, column: 0 },
        Delete      => MatrixIndex { row: 0, column: 0 },
    };
}

#[derive(Default)]
pub struct KeyboardMatrix([[bool; 8]; 8]);

impl KeyboardMatrix {
    pub fn num_rows(&self) -> usize {
        return self.0.len();
    }

    pub fn num_columns(&self) -> usize {
        return self.0[0].len();
    }
}

impl IndexMut<MatrixIndex> for KeyboardMatrix {
    fn index_mut(&mut self, idx: MatrixIndex) -> &mut Self::Output {
        &mut self.0[idx.row as usize][idx.column as usize]
    }
}

impl Index<MatrixIndex> for KeyboardMatrix {
    type Output = bool;
    fn index(&self, idx: MatrixIndex) -> &Self::Output {
        &self.0[idx.row as usize][idx.column as usize]
    }
}

impl IndexMut<C64Key> for KeyboardMatrix {
    fn index_mut(&mut self, key: C64Key) -> &mut Self::Output {
        self.index_mut(VIDEO_MATRIX_POS[key])
    }
}

impl Index<C64Key> for KeyboardMatrix {
    type Output = bool;
    fn index(&self, key: C64Key) -> &Self::Output {
        self.index(VIDEO_MATRIX_POS[key])
    }
}

impl<I> From<I> for KeyboardMatrix
where
    I: Iterator<Item = C64Key>,
{
    fn from(pressed_keys: I) -> Self {
        let mut matrix = KeyboardMatrix::default();

        for key in pressed_keys {
            matrix[VIDEO_MATRIX_POS[key]] = true;
        }

        matrix
    }
}

impl fmt::Debug for KeyboardMatrix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in self.0.iter() {
            for is_pressed in row {
                write!(f, "{}", *is_pressed as u8)?;
            }
            write!(f, "\n")?;
        }

        Ok(())
    }
}

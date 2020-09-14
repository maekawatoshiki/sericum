pub mod calc_spill_weight;
pub mod eliminate_fi;
pub mod inst;
// pub mod phi_elimination;
pub mod pro_epi_inserter;
pub mod regalloc;
pub mod replace_copy;
// pub mod validate_frame_index;
// pub mod replace_data;
pub mod abi;
pub mod inst_def;
pub mod live_interval_splitter;
pub mod register;
pub mod spiller;
pub use super::frame_object;

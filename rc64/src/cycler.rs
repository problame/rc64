use std::time::{Duration, Instant};

pub struct Config {
    pub guest_core_cps_hz: f64,
    pub report_interval: Option<Duration>,
}

#[derive(Debug, Default, Clone)]
pub struct Report {
    slow_cycles_total: u64,
}

#[derive(Debug)]
pub struct ReportDelta {
    pub slow_cycles: u64,
}

impl Report {
    pub fn delta(&self, other: &Report) -> Option<ReportDelta> {
        if self.slow_cycles_total < other.slow_cycles_total {
            None
        } else {
            Some(ReportDelta { slow_cycles: self.slow_cycles_total - other.slow_cycles_total })
        }
    }
}

pub struct Cycler {
    config: Config,

    last_report_at: Instant,
    next_report: Report,

    startup_at: Instant,
    startup_guest_cycles: u64,
}

impl Cycler {
    pub fn new(config: Config) -> Self {
        let now = Instant::now();
        Cycler {
            config,
            last_report_at: now,
            next_report: Report::default(),
            startup_at: now,
            startup_guest_cycles: 0,
        }
    }

    pub fn cycle(&mut self, guest_cycles: u64) -> Option<Report> {
        let now = Instant::now();

        let guest_uptime = now - self.startup_at;
        let sleeptime = ((guest_cycles - self.startup_guest_cycles) as f64 / self.config.guest_core_cps_hz)
            - guest_uptime.as_secs_f64();
        let need_sleep = sleeptime > 0.0;
        if need_sleep {
            spin_sleep::sleep(Duration::from_secs_f64(sleeptime));
        } else {
            self.next_report.slow_cycles_total = self.next_report.slow_cycles_total.wrapping_add(1);
        }

        if let Some(report_interval) = self.config.report_interval {
            if now - self.last_report_at > report_interval {
                self.last_report_at = now;
                return Some(self.next_report.clone());
            }
            // fallthrough
        }
        None
    }

    pub fn reset_startup(&mut self, guest_cycles: u64) {
        self.startup_at = Instant::now();
        self.startup_guest_cycles = guest_cycles;
    }
}

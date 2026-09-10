// Carbon Debt Explorer — frontend logic
// Loads data.json, renders two Highcharts plots, reacts to input changes.

const REGION_COLORS = {
  AFR: 'var(--r-afr)',
  EAS: 'var(--r-eas)',
  EUR: 'var(--r-eur)',
  FSU: 'var(--r-fsu)',
  LAM: 'var(--r-lam)',
  MEA: 'var(--r-mea)',
  NAM: 'var(--r-nam)',
  PAO: 'var(--r-pao)',
  PAS: 'var(--r-pas)',
  SAS: 'var(--r-sas)',
};

const REGION_NAMES = {
  AFR: 'Africa',
  EAS: 'East Asia',
  EUR: 'Europe',
  FSU: 'Former Soviet Union',
  LAM: 'Latin America',
  MEA: 'Middle East',
  NAM: 'North America',
  PAO: 'Pacific OECD',
  PAS: 'Other Pacific Asia',
  SAS: 'South Asia',
};

let DATA = null;
let STATE = {
  temperature: '1p5_50',
  startYear: 1990,
  scenario: 'E',
  capability: 'none',
};

// Resolve a CSS variable to its computed color (for Highcharts, which wants hex/rgb)
function resolveCssVar(cssVar) {
  const m = cssVar.match(/var\((--[^)]+)\)/);
  if (!m) return cssVar;
  return getComputedStyle(document.documentElement).getPropertyValue(m[1]).trim();
}

function resolveRegionColor(region) {
  return resolveCssVar(REGION_COLORS[region] || 'var(--primary)');
}

function fmtGt(v) {
  if (v === null || v === undefined) return '—';
  const sign = v > 0 ? '+' : '';
  return `${sign}${v.toFixed(1)}`;
}

function buildComboKey() {
  return `${STATE.temperature}|${STATE.startYear}|${STATE.scenario}|${STATE.capability}`;
}

// ------------------ Charts ------------------

function sharedHighchartsConfig() {
  const text = resolveCssVar('var(--text)');
  const dim = resolveCssVar('var(--text-dim)');
  const border = resolveCssVar('var(--border)');
  return {
    credits: { enabled: false },
    accessibility: { enabled: false },
    chart: {
      backgroundColor: 'transparent',
      style: { fontFamily: "'Source Sans 3', system-ui, sans-serif" },
    },
    xAxis: {
      gridLineColor: border,
      lineColor: border,
      tickColor: border,
      labels: { style: { color: dim, fontSize: '12px' } },
      title: { style: { color: dim, fontSize: '12px' } },
    },
    yAxis: {
      gridLineColor: border,
      lineColor: border,
      tickColor: border,
      labels: { style: { color: dim, fontSize: '12px' } },
      title: { style: { color: dim, fontSize: '12px' } },
    },
    legend: {
      itemStyle: { color: text, fontWeight: '500', fontSize: '12px' },
      itemHoverStyle: { color: resolveCssVar('var(--primary)') },
    },
    tooltip: {
      backgroundColor: resolveCssVar('var(--surface)'),
      borderColor: border,
      style: { color: text, fontSize: '12px' },
    },
    plotOptions: { series: { animation: { duration: 250 } } },
  };
}

function renderDebtScatter(combo) {
  const base = sharedHighchartsConfig();
  const primary = resolveCssVar('var(--primary)');
  const debt = resolveCssVar('var(--debt)');
  const regions = Object.keys(combo.carbon_debt);
  const window = `${STATE.startYear}–2050`;
  const series = regions.map((r) => {
    const ratio = combo.per_capita_ratio[r].Median;
    const yMed = combo.carbon_debt[r].Median;
    const yMax = combo.carbon_debt[r].Max;
    const yMin = combo.carbon_debt[r].Min;
    return {
      name: `${r} — ${REGION_NAMES[r]}`,
      color: resolveRegionColor(r),
      data: [{
        x: ratio,
        y: yMed,
        regionCode: r,
        regionName: REGION_NAMES[r],
        yMax, yMin,
      }],
      marker: { radius: 8, symbol: 'circle', lineWidth: 2, lineColor: resolveCssVar('var(--surface)') },
    };
  });
  return Highcharts.chart('chart-scatter', Highcharts.merge(base, {
    chart: { type: 'scatter', height: 420 },
    title: { text: null },
    xAxis: {
      title: { text: `Per-capita emissions relative to fair share (${window})` },
      plotLines: [{ color: debt, width: 1, value: 1, dashStyle: 'Dash', zIndex: 2, label: {
        text: 'Fair share', style: { color: debt, fontSize: '11px' }, y: 14, x: 5,
      }}],
    },
    yAxis: {
      title: { text: `Carbon debt (GtCO₂, positive = overdraft by ${STATE.startYear}–2050)` },
      plotLines: [{ color: resolveCssVar('var(--text-dim)'), width: 1, value: 0, zIndex: 2 }],
    },
    tooltip: {
      useHTML: true,
      formatter: function () {
        const p = this.point;
        const debtStr = fmtGt(p.y);
        const rangeStr = `[${fmtGt(p.yMin)}, ${fmtGt(p.yMax)}]`;
        return `<strong>${p.regionCode} — ${p.regionName}</strong><br/>
                Carbon debt: <b>${debtStr}</b> GtCO₂ <span style="color:${resolveCssVar('var(--text-dim)')}">${rangeStr}</span><br/>
                Emissions ratio: <b>${p.x.toFixed(2)}×</b> fair share`;
      },
    },
    series,
    plotOptions: {
      scatter: {
        dataLabels: {
          enabled: true,
          format: '{series.name}',
          style: {
            color: resolveCssVar('var(--text)'),
            textOutline: '2px ' + resolveCssVar('var(--surface)'),
            fontSize: '11px',
            fontWeight: '500',
          },
          formatter: function () { return this.point.regionCode; },
          align: 'left',
          verticalAlign: 'middle',
          x: 10,
        },
      },
    },
    legend: { enabled: false },
  }));
}

function renderEmissionsPaths(combo) {
  const base = sharedHighchartsConfig();
  const regions = Object.keys(combo.per_capita_emissions);
  const series = [];
  const startYear = STATE.startYear;
  regions.forEach((r) => {
    const color = resolveRegionColor(r);
    const d = combo.per_capita_emissions[r];
    const filteredHistorical = (d.historical || []).filter(([year]) => year >= startYear);
    if (filteredHistorical.length) {
      series.push({
        name: `${r} historical`,
        data: filteredHistorical,
        color: color,
        type: 'line',
        lineWidth: 1.5,
        marker: { enabled: false },
        showInLegend: false,
        linkedTo: `proj-${r}`,
        enableMouseTracking: true,
        states: { hover: { lineWidth: 2.5 } },
      });
    }
    if (d.projected && d.projected.length) {
      const rangeData = d.projected.map((row) => [row[0], row[1], row[3]]);
      const medianData = d.projected.map((row) => [row[0], row[2]]);
      series.push({
        id: `proj-${r}`,
        name: `${r} — ${REGION_NAMES[r]}`,
        data: medianData,
        color: color,
        type: 'line',
        lineWidth: 1.8,
        dashStyle: 'ShortDash',
        marker: { enabled: false },
        states: { hover: { lineWidth: 3 } },
      });
      series.push({
        name: `${r} range`,
        data: rangeData,
        type: 'arearange',
        color: color,
        fillOpacity: 0.12,
        lineWidth: 0,
        marker: { enabled: false },
        enableMouseTracking: false,
        showInLegend: false,
        linkedTo: `proj-${r}`,
      });
    }
  });
  return Highcharts.chart('chart-paths', Highcharts.merge(base, {
    chart: { type: 'line', height: 420, zoomType: 'x' },
    title: { text: null },
    xAxis: {
      title: { text: 'Year' },
      min: startYear,
      plotLines: [{ color: resolveCssVar('var(--text-dim)'), width: 1, value: 2023, dashStyle: 'Dash', zIndex: 2, label: {
        text: 'Scenario start (2023)', style: { color: resolveCssVar('var(--text-dim)'), fontSize: '11px' }, rotation: 0, y: -4, align: 'right', x: -6,
      }}],
    },
    yAxis: {
      title: { text: 'Per-capita CO₂ emissions (tCO₂ / person / year)' },
      plotLines: [{ color: resolveCssVar('var(--text-dim)'), width: 1, value: 0, zIndex: 2 }],
    },
    legend: {
      layout: 'horizontal',
      align: 'center',
      verticalAlign: 'bottom',
      maxHeight: 80,
      itemStyle: { fontSize: '11px' },
    },
    tooltip: {
      shared: false,
      useHTML: true,
      formatter: function () {
        return `<strong>${this.series.name}</strong><br/>Year ${this.x}: ${this.y.toFixed(2)} tCO₂/person`;
      },
    },
    series,
  }));
}

// ------------------ Summary strip ------------------

function renderSummary(combo) {
  const el = document.querySelector('.summary-strip');
  const regions = Object.keys(combo.carbon_debt);
  const debts = regions.map((r) => combo.carbon_debt[r].Median);
  const totalDebt = debts.filter((d) => d > 0).reduce((a, b) => a + b, 0);
  const totalCredit = debts.filter((d) => d < 0).reduce((a, b) => a + b, 0);
  const worst = regions.reduce((acc, r) =>
    combo.carbon_debt[r].Median > combo.carbon_debt[acc].Median ? r : acc, regions[0]);
  el.innerHTML = `
    <div class="stat">
      <div class="stat-label">Remaining budget (post-2022)</div>
      <div class="stat-value">${combo.rcb_gtco2}<span class="stat-unit">GtCO₂</span></div>
    </div>
    <div class="stat">
      <div class="stat-label">Total allocation pool</div>
      <div class="stat-value">${combo.total_budget_pool_gtco2.toFixed(0)}<span class="stat-unit">GtCO₂</span></div>
    </div>
    <div class="stat">
      <div class="stat-label">Sum of carbon debt</div>
      <div class="stat-value" style="color:var(--debt)">+${totalDebt.toFixed(0)}<span class="stat-unit">GtCO₂</span></div>
    </div>
    <div class="stat">
      <div class="stat-label">Sum of carbon credit</div>
      <div class="stat-value" style="color:var(--credit)">${totalCredit.toFixed(0)}<span class="stat-unit">GtCO₂</span></div>
    </div>
    <div class="stat">
      <div class="stat-label">Largest debt (region)</div>
      <div class="stat-value">${worst}<span class="stat-unit">+${combo.carbon_debt[worst].Median.toFixed(1)} GtCO₂</span></div>
    </div>
  `;
}

// ------------------ Wiring ------------------

function refresh() {
  if (!DATA) return;
  const key = buildComboKey();
  const combo = DATA.combinations[key];
  if (!combo) {
    console.warn('No combination for', key);
    return;
  }
  renderSummary(combo);
  renderDebtScatter(combo);
  renderEmissionsPaths(combo);
}

function initInputs() {
  const tempSel = document.getElementById('input-temp');
  DATA.inputs.temperature_goals.forEach((t) => {
    const opt = document.createElement('option');
    opt.value = t.key;
    opt.textContent = `${t.label} — ${t.rcb_gtco2} GtCO₂`;
    opt.value = t.key;
    tempSel.appendChild(opt);
  });
  tempSel.value = STATE.temperature;
  tempSel.addEventListener('change', (e) => {
    STATE.temperature = e.target.value;
    refresh();
  });

  const cite = document.getElementById('rcb-citation');
  if (cite && DATA.rcb_source) {
    const s = DATA.rcb_source;
    cite.innerHTML = `Budgets from <a href="${s.url}" target="_blank" rel="noopener">${s.citation}</a>, baseline ${s.baseline_year}.`;
  }

  const scenarioSel = document.getElementById('input-scenario');
  DATA.inputs.scenarios.forEach((s) => {
    const opt = document.createElement('option');
    opt.value = s.key;
    opt.textContent = s.label;
    scenarioSel.appendChild(opt);
  });
  scenarioSel.value = STATE.scenario;
  scenarioSel.addEventListener('change', (e) => {
    STATE.scenario = e.target.value;
    refresh();
  });

  const capSel = document.getElementById('input-capability');
  DATA.inputs.capabilities.forEach((c) => {
    const opt = document.createElement('option');
    opt.value = c.key;
    opt.textContent = c.label;
    capSel.appendChild(opt);
  });
  capSel.value = STATE.capability;
  capSel.addEventListener('change', (e) => {
    STATE.capability = e.target.value;
    refresh();
  });

  const years = DATA.inputs.start_years;
  const slider = document.getElementById('input-year');
  slider.min = 0;
  slider.max = years.length - 1;
  slider.step = 1;
  slider.value = years.indexOf(STATE.startYear);
  const out = document.getElementById('input-year-out');
  out.textContent = STATE.startYear;
  const ticks = document.getElementById('year-ticks');
  const legend = document.getElementById('year-legend');
  if (ticks) ticks.innerHTML = years.map((_, i) => `<option value="${i}"></option>`).join('');
  if (legend) {
    legend.innerHTML = years.map((y) => `<span>${y}</span>`).join('');
  }
  slider.addEventListener('input', (e) => {
    STATE.startYear = years[Number(e.target.value)];
    out.textContent = STATE.startYear;
    refresh();
  });
}

async function loadData() {
  const res = await fetch('assets/data.json');
  if (!res.ok) throw new Error(`Failed to load data.json: ${res.status}`);
  DATA = await res.json();
  initInputs();
  refresh();
  document.querySelector('.loader')?.remove();
}

function showError(msg) {
  let banner = document.getElementById('app-error-banner');
  if (!banner) {
    banner = document.createElement('div');
    banner.id = 'app-error-banner';
    banner.style.cssText = 'position:fixed;top:0;left:0;right:0;z-index:9999;' +
      'background:#b42318;color:#fff;padding:12px 16px;font:14px/1.4 system-ui,sans-serif;' +
      'box-shadow:0 2px 8px rgba(0,0,0,0.15);';
    document.body.appendChild(banner);
  }
  banner.innerHTML = `<strong>Failed to load:</strong> ${msg} ` +
    `<span style="opacity:0.8">(if you opened this as a local file, ` +
    `serve it via <code>make serve</code> — fetch() is blocked for file://)</span>`;
}

window.addEventListener('error', (e) => {
  showError(`JS error — ${e.message} (${e.filename}:${e.lineno})`);
});

window.addEventListener('unhandledrejection', (e) => {
  showError(`Unhandled promise rejection — ${e.reason}`);
});

document.addEventListener('DOMContentLoaded', () => {
  loadData().catch((err) => {
    console.error(err);
    showError(err.message);
    const loader = document.querySelector('.loader');
    if (loader) loader.textContent = `Failed to load data — ${err.message}`;
  });
});

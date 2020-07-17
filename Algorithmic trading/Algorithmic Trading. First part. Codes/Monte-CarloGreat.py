import os
import pandas as pd
import numpy as np
import random
import datetime
import plotly
import plotly.graph_objs as go
import plotly.figure_factory as ff
import ffn
from jinja2 import Environment, FileSystemLoader


class PerformanceReport:
    """ Report with performance stats for given strategy returns.
    """

    def __init__(self, infilename):
        self.infilename = infilename
        self.get_data()

    def get_data(self):
        basedir = os.path.abspath(os.path.dirname('__file__'))
        data_folder = os.path.join(basedir, 'data')
        data = pd.read_csv(os.path.join(data_folder, self.infilename), index_col='date',
                           parse_dates=True, dayfirst=True)
        self.equity_curve = data['equity_curve']

        if len(data.columns) > 1:
            self.benchmark_curve = data['benchmark']

    def generate_html(self):
        env = Environment(loader=FileSystemLoader('.'))
        template = env.get_template("templates/template.html")
        perf_chart = self.plot_performance_chart()
        drawdown_chart = self.plot_drawdown_chart()
        monthly_table = self.create_monthly_table(self.equity_curve.pct_change().dropna())
        equity_curve_ffn_stats = self.get_ffn_stats(self.equity_curve)

        benchmark_curve_ffn_stats = self.get_ffn_stats(self.benchmark_curve)
        kpi_table = self.create_kpi_table(equity_curve_ffn_stats)
        kpi_table_full = self.create_kpi_table_full([equity_curve_ffn_stats, benchmark_curve_ffn_stats])
        kpi_table_1, kpi_table_2, kpi_table_3, kpi_table_4, kpi_table_5 = self.split_kpi_table(kpi_table_full)
        daily_ret_hist = self.plot_daily_histogram()
        daily_ret_box = self.plot_daily_box()

        simulations = 250
        periods = 252
        monte_carlo_results, monte_carlo_hist, mc_max_dd_list = self.run_monte_carlo_parametric(
            self.equity_curve.pct_change().dropna(), periods, simulations)
        mc_chart = self.plot_mc_chart(monte_carlo_results)
        mc_dist_chart = self.plot_mc_dist_chart(monte_carlo_hist)
        mc_hist_chart = self.plot_mc_hist_chart(monte_carlo_hist)

        mc_best_worst_df = self.extract_best_worst(monte_carlo_results, periods)
        best_df_ffn_stats = self.get_ffn_stats(mc_best_worst_df['Best'])
        worst_df_ffn_stats = self.get_ffn_stats(mc_best_worst_df['Worst'])

        best_df_kpi = self.create_kpi_table(best_df_ffn_stats)
        worst_df_kpi = self.create_kpi_table(worst_df_ffn_stats)

        # mc_5perc,mc_95perc = self.calc_mc_var(monte_carlo_results,5)

        mc_dict_perf = self.mc_perf_probs(monte_carlo_results)
        mc_dict_dd = self.mc_dd_probs(mc_max_dd_list)

        html_out = template.render(perf_chart=perf_chart, drawdown_chart=drawdown_chart, monthly_table=monthly_table,
                                   kpi_table=kpi_table, kpi_table_1=kpi_table_1, kpi_table_2=kpi_table_2,
                                   kpi_table_3=kpi_table_3, kpi_table_4=kpi_table_4, kpi_table_5=kpi_table_5,
                                   daily_ret_hist=daily_ret_hist, daily_ret_box=daily_ret_box, mc_chart=mc_chart,
                                   mc_dist_chart=mc_dist_chart, mc_hist_chart=mc_hist_chart, best_df_kpi=best_df_kpi,
                                   worst_df_kpi=worst_df_kpi, mc_dict_perf=mc_dict_perf, mc_dict_dd=mc_dict_dd)
        return html_out

    def generate_html_report(self):
        """ Returns HTML report with analysis
        """
        html = self.generate_html()
        outputdir = "output"
        outfile = os.path.join(outputdir, 'report.html')
        file = open(outfile, "w")
        file.write(html)
        file.close()

    def rebase_series(self, series):
        return (series / series.iloc[0]) * 100

    def plot_performance_chart(self):

        # plotly combined equity chart
        trace_equity = go.Scatter(
            x=self.equity_curve.index.tolist(),
            y=self.rebase_series(self.equity_curve).values.tolist(),
            name='Strategy',
            yaxis='y2',
            line=dict(color=('rgb(22, 96, 167)')))

        trace_benchmark = go.Scatter(
            x=self.benchmark_curve.index.tolist(),
            y=self.rebase_series(self.benchmark_curve).values.tolist(),
            name='Benchmark',
            yaxis='y2',
            line=dict(color=('rgb(22, 96, 0)')))

        layout = go.Layout(
            autosize=True,
            legend=dict(orientation="h"),
            title='Performance Chart',
            yaxis=dict(
                title='Performance'))

        perf_chart = plotly.offline.plot({"data": [trace_equity, trace_benchmark],
                                          "layout": layout}, include_plotlyjs=False,
                                         output_type='div')

        return perf_chart

    def plot_drawdown_chart(self):

        # plotly combined drawdown chart
        trace_equity_drawdown = go.Scatter(
            x=self.equity_curve.to_drawdown_series().index.tolist(),
            y=self.equity_curve.to_drawdown_series().values.tolist(),
            name='Strategy',
            yaxis='y2',
            line=dict(color=('rgb(22, 96, 167)')))

        trace_benchmark_drawdown = go.Scatter(
            x=self.benchmark_curve.to_drawdown_series().index.tolist(),
            y=self.benchmark_curve.to_drawdown_series().values.tolist(),
            name='Benchmark',
            yaxis='y2',
            line=dict(color=('rgb(22, 96, 0)')))

        layout = go.Layout(
            autosize=True,
            legend=dict(orientation="h"),
            title='Drawdown Chart',
            yaxis=dict(
                title='Drawdown'))

        drawdown_chart = plotly.offline.plot({"data": [trace_equity_drawdown, trace_benchmark_drawdown],
                                              "layout": layout}, include_plotlyjs=False,
                                             output_type='div')

        return drawdown_chart

    def create_monthly_table(self, return_series):
        return_series.rename('weighted rets', inplace=True)
        returns_df_m = pd.DataFrame((return_series + 1).resample('M').prod() - 1)
        returns_df_m['Month'] = returns_df_m.index.month
        monthly_table = returns_df_m[['weighted rets', 'Month']].pivot_table(returns_df_m[['weighted rets', 'Month']],
                                                                             index=returns_df_m.index, columns='Month',
                                                                             aggfunc=np.sum).resample('A')
        monthly_table = monthly_table.aggregate('sum')
        monthly_table.columns = monthly_table.columns.droplevel()
        # replace full date in index column with just the correspnding year
        monthly_table.index = monthly_table.index.year
        monthly_table['YTD'] = ((monthly_table + 1).prod(axis=1) - 1)
        monthly_table = monthly_table * 100
        monthly_table.replace(0.0, "", inplace=True)
        # Replace integer column headings with MMM format
        monthly_table.columns = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
                                 'YTD']
        return monthly_table.round(2).fillna("").to_html(classes="table table-hover table-bordered table-striped")

    def get_ffn_stats(self, equity_series):
        equity_stats = equity_series.calc_stats()
        d = dict(equity_stats.stats)
        return d

    def create_kpi_table(self, ffn_dict):
        kpi_table = pd.DataFrame.from_dict(ffn_dict, orient='index')
        kpi_table.index.name = 'KPI'
        kpi_table.columns = ['Value']
        kpi_table2 = kpi_table.loc[['total_return', 'cagr',
                                    'daily_vol', 'max_drawdown', 'avg_drawdown']]  # .astype(float)
        kpi_table2['Value'] = pd.Series(["{0:.2f}%".format(val * 100) for val in kpi_table2['Value']],
                                        index=kpi_table2.index)
        kpi_table2.loc['avg_drawdown_days'] = kpi_table.loc['avg_drawdown_days']
        kpi_table2.loc['daily_sharpe'] = np.round(kpi_table.loc['daily_sharpe'].values[0], 2)
        return kpi_table2.to_html(classes="table table-hover table-bordered table-striped", header=False)

    def create_kpi_table_full(self, ffn_dict_list):
        df_list = [pd.DataFrame.from_dict(x, orient='index') for x in ffn_dict_list]
        kpi_table_full = pd.concat(df_list, axis=1)
        return kpi_table_full

    def split_kpi_table(self, kpi_table_full):
        kpi_table_1 = kpi_table_full.iloc[3:16].to_html(classes="table table-hover table-bordered table-striped",
                                                        header=False)
        kpi_table_2 = kpi_table_full.iloc[16:24].to_html(classes="table table-hover table-bordered table-striped",
                                                         header=False)
        kpi_table_3 = kpi_table_full.iloc[24:32].to_html(classes="table table-hover table-bordered table-striped",
                                                         header=False)
        kpi_table_4 = kpi_table_full.iloc[32:40].to_html(classes="table table-hover table-bordered table-striped",
                                                         header=False)
        kpi_table_5 = kpi_table_full.iloc[40:].to_html(classes="table table-hover table-bordered table-striped",
                                                       header=False)
        return kpi_table_1, kpi_table_2, kpi_table_3, kpi_table_4, kpi_table_5

    def run_monte_carlo_parametric(self, returns, trading_days, simulations):

        df_list = []
        result = []
        S = 100
        T = trading_days
        mu = returns.mean()
        vol = returns.std()
        dd_result = []
        for i in range(simulations):
            # create list of daily returns using random normal distribution
            daily_returns = np.random.normal(mu, vol, T) + 1

            # set starting price and create price series generated by above random daily returns
            price_list = [S]

            for x in daily_returns:
                price_list.append(price_list[-1] * x)
            df = pd.DataFrame(price_list)
            max_dd = ffn.calc_max_drawdown(df)
            dd_result.append(max_dd)
            df_list.append(df)
            result.append(price_list[-1])
        df_master = pd.concat(df_list, axis=1)
        df_master.columns = range(len(df_master.columns))

        return df_master, result, dd_result

    def extract_best_worst(self, monte_carlo_results, trading_days):
        today = datetime.datetime.today().strftime('%d/%m/%Y')
        date_range = pd.bdate_range(end=today, periods=trading_days + 1, freq='B', dayfirst=True)
        monte_carlo_results.columns = range(len(monte_carlo_results.columns))
        last_row = monte_carlo_results.iloc[-1]
        max_col = last_row.idxmax(axis=1)
        best_df = monte_carlo_results[max_col]
        min_col = last_row.idxmin(axis=1)
        worst_df = monte_carlo_results[min_col]

        best_df = best_df.to_frame().set_index(date_range)
        worst_df = worst_df.to_frame().set_index(date_range)

        mc_best_worst_df = pd.concat([best_df, worst_df], axis=1)
        mc_best_worst_df.columns = ['Best', 'Worst']

        return mc_best_worst_df

    def calc_mc_var(self, monte_carlo_results, confidence):

        mc_as_array = np.array(monte_carlo_results)
        mc_low_perc = round(((np.percentile(mc_as_array, 100 - confidence) / 100) - 1) * 100, 2)
        mc_high_perc = round(((np.percentile(mc_as_array, confidence) / 100) - 1) * 100, 2)

        return mc_low_perc, mc_high_perc

    def mc_perf_probs(self, monte_carlo_results):
        mc_as_array = np.array(monte_carlo_results)
        mc_5perc = round(((np.percentile(mc_as_array, 5) / 100) - 1) * 100, 2)
        mc_95perc = round(((np.percentile(mc_as_array, 95) / 100) - 1) * 100, 2)

        mc_1perc = round(((np.percentile(mc_as_array, 1) / 100) - 1) * 100, 2)
        mc_10perc = round(((np.percentile(mc_as_array, 10) / 100) - 1) * 100, 2)
        mc_20perc = round(((np.percentile(mc_as_array, 20) / 100) - 1) * 100, 2)
        mc_30perc = round(((np.percentile(mc_as_array, 30) / 100) - 1) * 100, 2)
        mc_40perc = round(((np.percentile(mc_as_array, 40) / 100) - 1) * 100, 2)
        mc_50perc = round(((np.percentile(mc_as_array, 50) / 100) - 1) * 100, 2)
        mc_60perc = round(((np.percentile(mc_as_array, 60) / 100) - 1) * 100, 2)
        mc_70perc = round(((np.percentile(mc_as_array, 70) / 100) - 1) * 100, 2)
        mc_80perc = round(((np.percentile(mc_as_array, 80) / 100) - 1) * 100, 2)
        mc_90perc = round(((np.percentile(mc_as_array, 90) / 100) - 1) * 100, 2)
        mc_99perc = round(((np.percentile(mc_as_array, 99) / 100) - 1) * 100, 2)

        mc_dict_perf = {
            'mc_1perc': mc_1perc,
            'mc_5perc': mc_5perc,
            'mc_10perc': mc_10perc,
            'mc_20perc': mc_20perc,
            'mc_30perc': mc_30perc,
            'mc_40perc': mc_40perc,
            'mc_50perc': mc_50perc,
            'mc_60perc': mc_60perc,
            'mc_70perc': mc_70perc,
            'mc_80perc': mc_80perc,
            'mc_90perc': mc_90perc,
            'mc_95perc': mc_95perc,
            'mc_99perc': mc_99perc
        }
        return mc_dict_perf

    def mc_dd_probs(self, mc_max_dd_list):
        mc_as_array_dd = np.array(mc_max_dd_list)
        mc_5perc_dd = round((np.percentile(mc_as_array_dd, 5)) * 100, 2)
        mc_95perc_dd = round((np.percentile(mc_as_array_dd, 95)) * 100, 2)

        mc_1perc_dd = round((np.percentile(mc_as_array_dd, 1)) * 100, 2)
        mc_10perc_dd = round((np.percentile(mc_as_array_dd, 10)) * 100, 2)
        mc_20perc_dd = round((np.percentile(mc_as_array_dd, 20)) * 100, 2)
        mc_30perc_dd = round((np.percentile(mc_as_array_dd, 30)) * 100, 2)
        mc_40perc_dd = round((np.percentile(mc_as_array_dd, 40)) * 100, 2)
        mc_50perc_dd = round((np.percentile(mc_as_array_dd, 50)) * 100, 2)
        mc_60perc_dd = round((np.percentile(mc_as_array_dd, 60)) * 100, 2)
        mc_70perc_dd = round((np.percentile(mc_as_array_dd, 70)) * 100, 2)
        mc_80perc_dd = round((np.percentile(mc_as_array_dd, 80)) * 100, 2)
        mc_90perc_dd = round((np.percentile(mc_as_array_dd, 90)) * 100, 2)
        mc_99perc_dd = round((np.percentile(mc_as_array_dd, 99)) * 100, 2)

        mc_dict_dd = {
            'mc_1perc_dd': mc_1perc_dd,
            'mc_5perc_dd': mc_5perc_dd,
            'mc_10perc_dd': mc_10perc_dd,
            'mc_20perc_dd': mc_20perc_dd,
            'mc_30perc_dd': mc_30perc_dd,
            'mc_40perc_dd': mc_40perc_dd,
            'mc_50perc_dd': mc_50perc_dd,
            'mc_60perc_dd': mc_60perc_dd,
            'mc_70perc_dd': mc_70perc_dd,
            'mc_80perc_dd': mc_80perc_dd,
            'mc_90perc_dd': mc_90perc_dd,
            'mc_95perc_dd': mc_95perc_dd,
            'mc_99perc_dd': mc_99perc_dd
        }
        return mc_dict_dd

    def plot_daily_histogram(self):

        trace0 = go.Histogram(x=self.equity_curve.pct_change().values,
                              name="Strategy",
                              opacity=0.75,
                              marker=dict(
                                  color=('rgb(22, 96, 167)')),
                              xbins=dict(
                                  size=0.0025
                              ))

        trace1 = go.Histogram(x=self.benchmark_curve.pct_change().values,
                              name="Benchmark",
                              opacity=0.75,
                              marker=dict(
                                  color=('rgb(22, 96, 0)')),
                              xbins=dict(
                                  size=0.0025
                              ))

        data = [trace0, trace1]

        layout = go.Layout(
            title='Histogram of Strategy and Benchmark Daily Returns',
            autosize=True,
            height=600,
            hovermode='closest',
            barmode='overlay'
        )

        daily_ret_hist = plotly.offline.plot({"data": data, "layout": layout},
                                             include_plotlyjs=False,
                                             output_type='div')

        return daily_ret_hist

    def plot_daily_box(self):
        trace0 = go.Box(y=self.equity_curve.pct_change().values,
                        name="Strategy",
                        marker=dict(
                            color=('rgb(22, 96, 167)')))

        trace1 = go.Box(y=self.benchmark_curve.pct_change().values,
                        name="Benchmark",
                        marker=dict(
                            color=('rgb(22, 96, 0)')))

        data = [trace0, trace1]

        layout = go.Layout(
            title='Boxplot of Strategy and Benchmark Daily Returns',
            autosize=True,
            height=600,
            yaxis=dict(
                zeroline=False
            )
        )

        box_plot = plotly.offline.plot({"data": data, "layout": layout}, include_plotlyjs=False,
                                       output_type='div')

        return box_plot

    def get_random_rgb(self):
        col = 'rgb' + str((random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)))
        return col

    def plot_mc_chart(self, monte_carlo_results):
        mc_trace_list = []
        for col in monte_carlo_results.columns:
            rgb = self.get_random_rgb()
            trace = go.Scatter(
                x=monte_carlo_results.index.tolist(),
                y=monte_carlo_results[col].values.tolist(),
                name='mc data',
                yaxis='y2',
                line=dict(color=(rgb)))
            mc_trace_list.append(trace)

        layout_mc = go.Layout(
            title='Monte Carlo Parametric',
            yaxis=dict(title='Equity'),
            autosize=True,
            height=600,
            showlegend=False,
            hovermode='closest'
        )

        mc_chart = plotly.offline.plot({"data": mc_trace_list,
                                        "layout": layout_mc}, include_plotlyjs=False,
                                       output_type='div')

        return mc_chart

    def plot_mc_dist_chart(self, monte_carlo_hist):

        fig = ff.create_distplot([monte_carlo_hist], group_labels=['Strategy Monte Carlo Returns'], show_rug=False)
        fig['layout'].update(autosize=True, height=600,
                             title="Parametric Monte Carlo Simulations <br> (Distribution Plot - Ending Equity)",
                             showlegend=False)
        dist_plot_MC_parametric = plotly.offline.plot(fig, include_plotlyjs=False,
                                                      output_type='div')
        return dist_plot_MC_parametric

    def plot_mc_hist_chart(self, monte_carlo_hist):

        layout_mc = go.Layout(
            title="Parametric Monte Carlo Simulations <br> (Histogram - Ending Equity)",
            autosize=True,
            height=600,
            showlegend=False,
            hovermode='closest'
        )

        data_mc_hist = [go.Histogram(x=monte_carlo_hist,
                                     histnorm='probability')]

        mc_hist = plotly.offline.plot({"data": data_mc_hist, "layout": layout_mc}, include_plotlyjs=False,
                                      output_type='div')
        return mc_hist


if __name__ == "__main__":
    report = PerformanceReport('data.csv')
    report.generate_html_report()
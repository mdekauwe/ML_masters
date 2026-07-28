#!/usr/bin/env python

"""
Using a flux site, calculate the climatic water deficit (CWD)

Martin De Kauwe
28th July 2026
"""

import xarray as xr
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys

def latent_heat_vaporisation(tair):
    """Latent heat of vaporisation (MJ kg-1)."""
    return (2.501 - 0.002361 * tair)

def le_to_et_mm(LE, dt=1800.0, tair=None):
    """
    Convert latent heat flux (W m-2) to ET (mm per timestep)

    dt = 1800 is 30 min
    """

    if tair is None:
        lv = 2.45e6  # J kg-1
    else:
        lv = latent_heat_vaporisation(tair) * 1e6 # J kg-1

    et = LE * dt / lv

    return et

def calc_pet_priestley_taylor(tair, pressure, rn):
    """
    Priestley-Taylor potential ET.

    Parameters
    ----------
    tair : degC
    pressure : kPa
    rn : MJ m-2 day-1

    Returns
    -------
    PET (mm day-1)

    """

    alpha = 1.26
    cp = 1.013e-3      # MJ kg-1 degC-1
    epsilon = 0.622
    G = 0.0            # Daily ground heat flux assumed negligible

    if tair is None:
        lv = 2.45         # MJ kg-1
    else:
        lv = latent_heat_vaporisation(tair) # MJ kg-1

    # Psychrometric constant (kPa degC-1)
    gamma = (cp * pressure) / (epsilon * lv)

    # Saturation vapour pressure (kPa)
    es = 0.6108 * np.exp((17.27 * tair) / (tair + 237.3))

    # Slope of saturation vapour pressure curve (kPa degC-1)
    delta = (4098.0 * es) / ((tair + 237.3) ** 2)

    # mm day-1
    pet = alpha * (delta / (delta + gamma)) * ((rn - G) / lv)

    return np.maximum(pet, 0.0)

def calc_pet_fao56(tair, qair, sw_d, ws, lw_d, pressure, rn):
    """
    Daily PET (mm day-1) using FAO56 Penman-Monteith.

    Parameters
    ----------
    tair : degC
    qair : kg kg-1
    sw : MJ m-2 day-1
    ws : m s-1
    pressure : kPa

    Returns
    -------
    PET (mm day-1)
    """

    # saturation vapour pressure
    es = 0.6108 * np.exp((17.27 * tair) / (tair + 237.3))

    # actual vapour pressure
    #ea = es * rh / 100.
    ea = (qair * pressure) / (0.622 + 0.378 * qair)
    delta = (4098.0 * es / ((tair + 237.3) ** 2))
    gamma = 0.000665 * pressure

    # mm day-1
    pet = (
        0.408 * delta * rn
        + gamma * (900.0 / (tair + 273.0))
        * ws * (es - ea)
    ) / (
        delta + gamma * (1.0 + 0.34 * ws)
    )

    return np.maximum(pet, 0.0)

def calc_rnet(tair, sw_d, lw_d, albedo=0.15):
    """
    Calculate Rnet

    Parameters
    ----------
    tair : degC
    sw : MJ m-2 day-1
    LWup : MJ m-2 day-1

    Returns
    -------
    Rnet (MJ m-2 day-1)
    """
    #albedo = 0.15 # treee
    #albedo = 0.23 # grass
    Rns = (1.0 - albedo) * sw_d

    sigma = 5.67e-8
    epsilon = 0.97
    tair_K = tair + 273.15
    LWup = epsilon * sigma * tair_K**4
    LWup = LWup * 86400.0 / 1e6  # MJ m-2 day-1

    Rn = Rns + (lw_d - LWup)

    return (Rn)

if __name__ == "__main__":

    met_fn = "/Users/xj21307/research/Alice_Holt/data/UK-Ham_2002-2003_Met.nc"
    flx_fn = "/Users/xj21307/research/Alice_Holt/data/alice_holt_flux_2022.nc"
    met = xr.open_dataset(met_fn)
    flx = xr.open_dataset(flx_fn)


    tair_30min = met["Tair"].squeeze(drop=True) - 273.15
    tair_d = met["Tair"].squeeze(drop=True).resample(time="D").mean() - 273.15
    #rh_d = met[RH_VAR].resample(time="D").mean()
    qair_d = met["Qair"].squeeze(drop=True).resample(time="D").mean()
    wind_d = met["Wind"].squeeze(drop=True).resample(time="D").mean()
    pressure_d = (met["Psurf"].squeeze(drop=True).resample(time="D").mean() / 1000)


    # convert from W m-2 to MJ m-2 day-1
    # MJ d-1 = W m-2 * 86400 / 1e6
    sw_d = (met["SWdown"].squeeze(drop=True).resample(time="D").mean() * 86400.0 / 1e6)
    lw_d = (met["LWdown"].squeeze(drop=True).resample(time="D").mean() * 86400.0 / 1e6)


    le = flx["Qle"].squeeze(drop=True) # keep in 30 mins
    et_30min = le_to_et_mm(le, dt=1800, tair=tair_30min) #dt=30min=1800
    AET_d = (et_30min.resample(time="D").sum())
    AET_d.name = "AET"


    Rn = calc_rnet(tair_d, sw_d, lw_d, albedo=0.15)
    #Rn.groupby("time.month").mean().plot()

    PET_d = xr.apply_ufunc(calc_pet_priestley_taylor, tair_d, pressure_d, Rn)
    PET_d.name = "PET"

    #PET_d = xr.apply_ufunc(calc_pet_fao56, tair_d, qair_d, sw_d, wind_d, lw_d,
    #                       pressure_d)
    PET_d.name = "PET"

    AET_d, PET_d = xr.align(AET_d, PET_d)
    CWD = PET_d - AET_d
    #CWD_plot = CWD.clip(min=0)
    CWD_plot = CWD
    CWD_plot.name = "CWD"

    # cumulative CWD
    #CWD_cum = CWD.cumsum()
    CWD_pos = CWD.clip(min=0)
    CWD_cum = (CWD_pos.groupby("time.year").cumsum(dim="time"))
    CWD_cum.name = "CWD_cumulative"

    """
    fig, ax = plt.subplots(figsize=(10,4))

    PET_d.plot(ax=ax, label="PET")
    AET_d.plot(ax=ax, label="AET")

    ax.legend()
    plt.show()
    sys.exit()
    """

    fig, axes = plt.subplots(2, 1, figsize=(10, 8), sharex=True,
                             constrained_layout=True)


    CWD_plot.plot(ax=axes[0], color="0.5", lw=0.8, alpha=0.8, label="Daily")

    # 7-day running mean
    CWD_plot.rolling(time=7, center=True).mean().plot(ax=axes[0],color="firebrick",
                                                 lw=2, label="7-day mean",)

    # Shade positive deficits
    axes[0].fill_between(CWD_plot.time, 0, CWD, where=CWD > 0, color="firebrick",
                         alpha=0.25)

    axes[0].axhline(0, color="black", lw=1)

    axes[0].set_ylabel("Daily CWD (mm d$^{-1}$)")
    axes[0].legend(frameon=False)

    # Over the summer, atmospheric demand exceeded ecosystem water supply by
    # about 400 mm

    axes[1].fill_between(CWD_cum.time, 0, CWD_cum, color="forestgreen", alpha=0.25)
    CWD_cum.plot(ax=axes[1], color="forestgreen", lw=2.5)
    axes[1].set_ylabel("Cumulative CWD (mm)")

    # Highlight summers
    years = np.unique(CWD.time.dt.year)

    for year in years:

        start = np.datetime64(f"{year}-06-01")
        end   = np.datetime64(f"{year}-08-31")

        for ax in axes:
            ax.axvspan(start, end, color="gold", alpha=0.08, zorder=0)

    plt.show()

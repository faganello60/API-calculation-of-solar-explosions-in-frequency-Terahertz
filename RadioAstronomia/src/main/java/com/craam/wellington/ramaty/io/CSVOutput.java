package com.craam.wellington.ramaty.io;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.swing.text.DefaultEditorKit.BeepAction;

import com.craam.wellington.interfaces.IAlgorithm;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.ramaty.Ramaty;
import com.craam.wellington.ramaty.Ramaty.Frequency;
import com.craam.wellington.statics.Constants;
import com.tcc.faganello.jsonOutPut.ISRCSRModel;

public class CSVOutput implements IOutput {
	
	String filename;
	Ramaty ramaty;
	InputData params;
	List<Frequency> frequencies;
	
	//Data Flags
	Boolean extraordinaryModeOnly=false, ordinaryModeOnly=false;
	Boolean exportIndex=true, exportFrequency=true, exportEmissivity=true, exportAbsorption=true, exportPolarization=true, exportFlux=true, exportTotalFluxOnly=false, exportFluxPolarization=true;
	Boolean exportFluxSFU=false, exportFreqHz=false;
	
	public CSVOutput(String filename){
		this.filename = filename;
	}
	
	public Boolean generateOutput(IAlgorithm ramaty){
		this.ramaty = (Ramaty) ramaty;
		this.frequencies = (this.ramaty).getFrequencies();
		this.params = this.ramaty.getInputParams();
		
		PrintWriter printer;
		Iterator<Frequency> i;
		int k=1;
		
		try {
			printer = new PrintWriter(this.filename, "UTF-8");
			String parameters = String.format("ex=%.2e, anor=%.2e, bmag=%.2e, angle=%d, theta=%.2e, j1=%d, j2=%d, kf=%d, etr=%.2e, alpha=%.2e", params.getEnergyEspectrum(), params.getElectronNumber(), params.getMagneticField(), params.getViewAngle(), params.getSourceSize(), params.getEnergyLower(), params.getEnergyUpper(), params.getHighestFrequency(), params.getEnergyTreshould(), params.getAlpha());
			printer.println(parameters);
			
			String header = "";
			if(exportIndex) header += "Index";
			if(exportFrequency) header += ";Frequency";
			if(exportEmissivity && !ordinaryModeOnly) header += ";Emissivity x-mode";
			if(exportEmissivity && !extraordinaryModeOnly) header += ";Emissivity o-mode";
			if(exportAbsorption && !ordinaryModeOnly) header += ";Absorption x-mode";
			if(exportAbsorption && !extraordinaryModeOnly) header += ";Absorption o-mode";
			if(exportPolarization) header += "Polarization;";
			if(exportFlux && !ordinaryModeOnly && !exportTotalFluxOnly) header += ";Flux x-mode";
			if(exportFlux && !extraordinaryModeOnly && !exportTotalFluxOnly) header += ";Flux o-mode";
			if(exportFlux && !ordinaryModeOnly && !extraordinaryModeOnly) header += ";Flux total";
			if(exportFluxPolarization) header += ";Flux Polarization";
			printer.println(header);

			i = frequencies.iterator();
			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();

				if(exportIndex) row += String.format("%d", freq.getFrequencyIndex());
				if(exportFrequency) row += (exportFreqHz) ? String.format(";%.16e", freq.getFrequency() * Constants.FFB_TO_HZ * params.getMagneticField()) : String.format(";%.16e", freq.getFrequency());
				if(exportEmissivity && !ordinaryModeOnly) row += String.format(";%.16e", freq.getExtraordEmissivity());
				if(exportEmissivity && !extraordinaryModeOnly) row += String.format(";%.16e", freq.getOrdEmissivity());
				if(exportAbsorption && !ordinaryModeOnly) row += String.format(";%.16e", freq.getExtraordAbsorption());
				if(exportAbsorption && !extraordinaryModeOnly) row += String.format(";%.16e", freq.getOrdAbsorption());
				if(exportPolarization) row += String.format(";%.16e", freq.getStokesAndPolarization().getEmissivityPolarization());
				if(exportFlux && !ordinaryModeOnly && !exportTotalFluxOnly) row += (exportFluxSFU) ? String.format(";%.16e", (freq.getStokesAndPolarization().getPhiExtraord() > 0) ? freq.getStokesAndPolarization().getPhiExtraord() * Constants.ERGCMSHZ_TO_SFU : 1e-23) : String.format(";%.16e;", (freq.getStokesAndPolarization().getPhiExtraord() > 0) ? freq.getStokesAndPolarization().getPhiExtraord() : 1e-23); 
				if(exportFlux && !extraordinaryModeOnly && !exportTotalFluxOnly) row += (exportFluxSFU) ? String.format(";%.16e", (freq.getStokesAndPolarization().getPhiOrd() > 0) ? freq.getStokesAndPolarization().getPhiOrd() * Constants.ERGCMSHZ_TO_SFU : 1e-23) : String.format(";%.16e;", (freq.getStokesAndPolarization().getPhiOrd() > 0) ? freq.getStokesAndPolarization().getPhiOrd() : 1e-23);
				if(exportFlux && !ordinaryModeOnly && !extraordinaryModeOnly) row += (exportFluxSFU) ? String.format(";%.16e", (freq.getStokesAndPolarization().getPhiTotal() > 0) ? freq.getStokesAndPolarization().getPhiTotal() * Constants.ERGCMSHZ_TO_SFU : 1e-23) : String.format(";%.16e;", (freq.getStokesAndPolarization().getPhiTotal() >0) ? freq.getStokesAndPolarization().getPhiTotal() : 1e-23);
				if(exportFluxPolarization)  row += String.format(";%.16e", freq.getStokesAndPolarization().getFluxPolarization());
				printer.println(row);
			}


			printer.close();
		} catch (FileNotFoundException | UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		
		return true;
	}
	
	public CSVOutput setExtraordinaryModeOnly(Boolean extraordinaryModeOnly) {
		this.extraordinaryModeOnly = extraordinaryModeOnly;
		return this;
	}

	public CSVOutput setExportTotalFluxOnly(Boolean exportTotalFluxOnly) {
		this.exportTotalFluxOnly = exportTotalFluxOnly;
		return this;
	}

	public CSVOutput setOrdinaryModeOnly(Boolean ordinaryModeOnly) {
		this.ordinaryModeOnly = ordinaryModeOnly;
		return this;
	}

	public CSVOutput setExportIndex(Boolean exportIndex) {
		this.exportIndex = exportIndex;
		return this;
	}

	public CSVOutput setExportFrequency(Boolean exportFrequency) {
		this.exportFrequency = exportFrequency;
		return this;
	}

	public CSVOutput setExportEmissivity(Boolean exportEmissivity) {
		this.exportEmissivity = exportEmissivity;
		return this;
	}

	public CSVOutput setExportAbsorption(Boolean exportAbsorption) {
		this.exportAbsorption = exportAbsorption;
		return this;
	}

	public CSVOutput setExportPolarization(Boolean exportPolarization) {
		this.exportPolarization = exportPolarization;
		return this;
	}

	public CSVOutput setExportFlux(Boolean exportFlux) {
		this.exportFlux = exportFlux;
		return this;
	}

	public CSVOutput setExportFreqHz(Boolean exportFreqHz) {
		this.exportFreqHz = exportFreqHz;
		return this;
	}

	public CSVOutput setExportFluxPolarization(Boolean exportFluxPolarization) {
		this.exportFluxPolarization = exportFluxPolarization;
		return this;
	}

	public CSVOutput setExportFluxSFU(Boolean exportFluxSFU) {
		this.exportFluxSFU = exportFluxSFU;
		return this;
	}

	@Override
	public ISRCSRModel getResult() {
		// TODO Auto-generated method stub
		return null;
	}
	
}

package com.craam.wellington.ramaty.io;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import com.craam.wellington.interfaces.IAlgorithm;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.ramaty.Ramaty;
import com.craam.wellington.ramaty.Ramaty.Frequency;
import com.craam.wellington.statics.Constants;
import com.tcc.faganello.jsonOutPut.ISRCSRModel;

public class DataTableOutput implements IOutput {
	
	String filename;
	Ramaty ramaty;
	InputData params;
	List<Frequency> frequencies;
	
	public DataTableOutput(String filename){
		this.filename = filename;
		System.out.println(this.filename);
	}
	
	public Boolean generateOutput(IAlgorithm ramaty){
		this.ramaty = (Ramaty) ramaty;
		this.frequencies = this.ramaty.getFrequencies();
		this.params = this.ramaty.getInputParams();
		
		PrintWriter printer;
		Iterator<Frequency> i;
		int k=1;
		
		try {
			printer = new PrintWriter(this.filename, "UTF-8");
			printer.println(Constants.DATA_TABLE_REPORT_LINE);
			printer.println(Constants.DATA_TABLE_REPORT_HEADER);
			printer.println(Constants.DATA_TABLE_REPORT_LINE);			
			printer.print(Constants.DATA_TABLE_REPORT_PARAMS);
			printer.print("ex=" + params.getEnergyEspectrum());
			printer.print("\t\tanor=" + params.getElectronNumber());
			printer.print("\t\tbmag=" + params.getMagneticField());
			printer.print("\t\ttheta=" + params.getSourceSize());
			printer.print("\t\tj1=" + params.getEnergyLower());
			printer.print("\t\tj2=" + params.getEnergyUpper());
			printer.print("\t\tkf=" + params.getHighestFrequency());
			printer.print("\t\tetr=" + params.getEnergyTreshould());
			printer.println("\t\taplha=" + params.getAlpha());
			printer.println(Constants.DATA_TABLE_REPORT_UNITS);
			printer.println(Constants.DATA_TABLE_REPORT_LINE);
			printer.println(Constants.DATA_TABLE_REPORT_COLUMNS1);
			
			i = frequencies.iterator();
			while(i.hasNext()){
				Frequency freq = (Frequency) i.next();
				printer.print(String.format("%.16e", freq.getFrequency()) + "\t\t");
				printer.print(String.format("%.16e", freq.getOrdEmissivity()) + "\t\t");
				printer.print(String.format("%.16e", freq.getExtraordEmissivity()) + "\t\t");
				printer.print(String.format("%.16e", freq.getOrdAbsorption()) + "\t\t");
				printer.print(String.format("%.16e", freq.getExtraordAbsorption()) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getEmissivityPolarization()) + "\t\t");
				printer.println(k++);
			}
			
			i = frequencies.iterator();
			k =1;
			
			printer.println(Constants.DATA_TABLE_REPORT_LINE);
			printer.println(Constants.DATA_TABLE_REPORT_COLUMNS2);

			while(i.hasNext()){
				Frequency freq = (Frequency) i.next();
				printer.print(String.format("%.16e", freq.getFrequency() * Constants.FFB_TO_HZ * params.getMagneticField()) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getPhiOrd()) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getPhiExtraord()) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getPhiTotal()) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getPhiTotal() * Constants.ERGCMSHZ_TO_SFU) + "\t\t");
				printer.print(String.format("%.16e", freq.getStokesAndPolarization().getFluxPolarization()) + "\t\t");
				printer.println(k++);
			}

			printer.close();
		} catch (FileNotFoundException | UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		
		return true;
	}

	@Override
	public ISRCSRModel getResult() {
		// TODO Auto-generated method stub
		return null;
	}
	
}

package com.tcc.faganello.jsonOutPut;

import java.util.Iterator;
import java.util.List;

import com.craam.wellington.interfaces.IAlgorithm;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.isrcsr.ISRCSR;
import com.craam.wellington.isrcsr.Source;
import com.craam.wellington.isrcsr.ISRCSR.Frequency;
import com.craam.wellington.isrcsr.io.InputData;
import com.craam.wellington.statics.Constants;


/**
 * Essa classe é uma adaptação da CSVOutput.java
 * A classe original gerava vários arquivos em csv
 * A função dessa nova classe é gerar um objeto e retorna-lo na forma json
 */


public class JsonOutput implements IOutput {
	
	String filename = "TESTE";
	String filenameCompact, filenameExtended, filenameISR, filenameHigh, filenameLow, filenameCSR, filenameTotal;
	ISRCSR isrcsr;
	InputData params;
	List<Frequency> frequencies;
	ISRCSRModel result;
	
	//Data Flags
	Boolean exportFrequency=true;
	
	public JsonOutput(){
		
	}
	
	public Boolean generateOutput(IAlgorithm isrcsr){
		this.result = new ISRCSRModel();
		this.isrcsr = (ISRCSR) isrcsr;
		this.frequencies = this.isrcsr.getFrequencies();
		this.params = this.isrcsr.getInputParams();
		generateSourceOutput(params.getCompactSource(), frequencies, this.filenameCompact,true);
		generateSourceOutput(params.getExtendedSource(), frequencies, this.filenameExtended,false);
		generateISROutput(frequencies, this.filenameISR);
		generateCSROutput(frequencies, this.filenameCSR);
		generateHighOutput(frequencies, this.filenameHigh);
		generateLowOutput(frequencies, this.filenameLow);
		generateTotalOutput(frequencies, this.filenameTotal);
		System.out.println("Tudo Certo");
		return true;
	}
	
	public ISRCSRModel getResult(){
		return result;
	}
	
	
	public JsonOutput setExportFrequency(Boolean exportFrequency) {
		this.exportFrequency = exportFrequency;
		return this;
	}
	
	private void generateSourceOutput(Source s, List<Frequency> frequencies, String filename, boolean compact){
		Iterator<Frequency> i = frequencies.iterator();
		

			String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";
			

			i = frequencies.iterator();

			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				if(exportFrequency)  frequency = s.getFrequency(freq.getFrequencyIndex()).getFrequencyValue();
				                     flux 	   = s.getFrequency(freq.getFrequencyIndex()).getFluxes().getPhiTotal() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				System.out.println("Freq "+frequency);
				System.out.println("Flux "+flux);
				
				if (compact){
					this.result.compact.addFlux(flux);
					this.result.compact.addFrequency(frequency);
				}
				else{
					this.result.extended.addFlux(flux);
					this.result.extended.addFrequency(frequency);
				}
				
			}
	}

	private void generateISROutput(List<Frequency> frequencies, String filename){
		
		Iterator<Frequency> i = frequencies.iterator();
		
		String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";
			i = frequencies.iterator();
			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				
				if(exportFrequency) frequency = (freq.getFrequencyValue());
				                    flux = freq.getPhiISR() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				
				System.out.println("Freq "+frequency);
				System.out.println("Flux "+flux);
				this.result.isr.addFrequency(frequency);
				this.result.isr.addFlux(flux);
			}
			
			
		 
	}
	
	
	private void generateCSROutput(List<Frequency> frequencies, String filename){
		Iterator<Frequency> i = frequencies.iterator();
			String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";
			

			i = frequencies.iterator();

			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				
				if(exportFrequency) frequency =  freq.getFrequencyValue();
				                    flux = freq.getPhiCSR() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				
				System.out.println("Freq "+frequency);
				System.out.println("Flux "+flux);
				this.result.csr.addFrequency(frequency);
				this.result.csr.addFlux(flux);
			} 
	}

	private void generateHighOutput(List<Frequency> frequencies, String filename){
		Iterator<Frequency> i = frequencies.iterator();
		
		
			String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";
			

			i = frequencies.iterator();

			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				
				if(exportFrequency) frequency = freq.getFrequencyValue();
				                    flux = freq.getPhiHigh() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				this.result.high.addFrequency(frequency);
				this.result.high.addFlux(flux);
			}
	}

	private void generateLowOutput(List<Frequency> frequencies, String filename){
		Iterator<Frequency> i = frequencies.iterator();
		
			String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";

			i = frequencies.iterator();

			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				if(exportFrequency) frequency = freq.getFrequencyValue();
				                    flux = freq.getPhiLow() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				this.result.low.addFrequency(frequency);
				this.result.low.addFlux(flux);                    
				            
			}
	}

	private void generateTotalOutput(List<Frequency> frequencies, String filename){
		Iterator<Frequency> i = frequencies.iterator();

			String header = "";
			if(exportFrequency) header += "Frequency;";
			header += "Flux";

			i = frequencies.iterator();

			while(i.hasNext()){
				String row = "";
				Frequency freq = (Frequency) i.next();
				Double frequency = 0.0000000000000000;
				Double flux = 0.0000000000000000;
				if(exportFrequency) frequency = freq.getFrequencyValue();
				                    flux = freq.getPhiTotal() * Constants.ERGCMSHZ_TO_SFU+1e-23;
				this.result.total.addFrequency(frequency);
				this.result.total.addFlux(flux);
			}
			
			
		
	} 
}

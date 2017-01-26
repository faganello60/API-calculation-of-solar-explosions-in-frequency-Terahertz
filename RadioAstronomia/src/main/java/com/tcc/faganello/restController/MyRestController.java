package com.tcc.faganello.restController;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.craam.wellington.interfaces.IInput;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.isrcsr.ISRCSR;

import java.util.HashMap;
import com.tcc.faganello.jsonOutPut.ISRCSRModel;
import com.tcc.faganello.jsonOutPut.JsonOutput;



@RestController
public class MyRestController {
	@RequestMapping("/test")
	public ISRCSRModel test(){
		IInput<com.craam.wellington.isrcsr.io.InputData> isrcsrInput = new com.craam.wellington.isrcsr.io.ManualInput(new HashMap<String, Double>(){{
			put("ex", 2.5);
			put("ntotal", 1e35);
			put("bmagco", 1e3);
			put("bmagex", 1e3);
			put("angle", 45.);
			put("scsize", 0.5);
			put("scheight", 1e6);
			put("sesize", 20.);
			put("seheight", 1e9);
			put("j1", 15.);
			put("j2", 80.);
			put("etr", 2.5);
			put("npco", 9.72e-10);
			put("npex", 9.72e-10);
			put("ecsr", 5.);
			put("xnisrex", 0.999);
			put("xncsr", 5e-15);
			put("tb", 70.0e-12);
			put("kf",701.);														
			}});
		//IOutput isrcsrOutput = new com.craam.wellington.isrcsr.io.CSVOutput("isrcsr.out");
		IOutput isrcsrOutput = new JsonOutput();
		ISRCSR isrcsr = new ISRCSR();
		ISRCSRModel teste =  isrcsr.myContructor(isrcsrInput, isrcsrOutput);
		
		return teste;
	}
	
	@RequestMapping(value="/isrcsr",params = {"ex","ntotal","bmagco","bmagex","angle",
											  "scsize","scheight","sesize","seheight",
											  "j1","j2","etr","npco","npex","ecsr","xnisrex",
											  "xncsr","tb","kf"})
	public @ResponseBody Object tests(@RequestParam(value = "ex") 		final       double ex,
									  @RequestParam(value = "ntotal") 	final   	double ntotal,
									  @RequestParam(value = "bmagco") 	final   	double bmagco,
									  @RequestParam(value = "bmagex") 	final   	double bmagex,
									  @RequestParam(value = "angle") 	final    	double angle,
									  @RequestParam(value = "scsize") 	final   	double scsize,
									  @RequestParam(value = "scheight") final 		double scheight,
									  @RequestParam(value = "sesize") final   double sesize,
									  @RequestParam(value = "seheight") final double seheight,
									  @RequestParam(value = "j1") final       double j1,
									  @RequestParam(value = "j2") final       double j2,
									  @RequestParam(value = "etr") final      double etr,
									  @RequestParam(value = "npco") final     double npco,
									  @RequestParam(value = "npex") final 	double npex,
									  @RequestParam(value = "ecsr") final 	double ecsr,
									  @RequestParam(value = "xnisrex") final  double xnisrex,
									  @RequestParam(value = "xncsr") final    double xncsr,
									  @RequestParam(value = "tb") final 		double tb,
									  @RequestParam(value = "kf") final 		double kf){
		
		IInput<com.craam.wellington.isrcsr.io.InputData> isrcsrInput = new com.craam.wellington.isrcsr.io.ManualInput(new HashMap<String, Double>(){{
			put("ex", ex);
			put("ntotal", ntotal);
			put("bmagco", bmagco);
			put("bmagex", bmagex);
			put("angle", angle);
			put("scsize", scsize);
			put("scheight", scheight);
			put("sesize", sesize);
			put("seheight", seheight);
			put("j1", j1);
			put("j2", j2);
			put("etr", etr);
			put("npco", npco);
			put("npex", npex);
			put("ecsr", ecsr);
			put("xnisrex", xnisrex);
			put("xncsr", xncsr);
			put("tb", tb);
			put("kf",kf);														
			}});
		//http://localhost:8080/isrcsr?ex=2.5&ntotal=1e35&bmagco=1e3&bmagex=1e3&angle=45.0&scsize=0.5&scheight=1e6&sesize=20.0&seheight=1e9&j1=15.0&j2=80.0&etr=2.5&npco=9.72e-10&npex=9.72e-10&ecsr=5.0&xnisrex=0.999&xncsr=5e-15&tb=70.0e-12&kf=701.0
	
		
		//IOutput isrcsrOutput = new com.craam.wellington.isrcsr.io.CSVOutput("isrcsr.out");
		IOutput isrcsrOutput = new JsonOutput();
		ISRCSR isrcsr = new ISRCSR();
		ISRCSRModel teste =  isrcsr.myContructor(isrcsrInput, isrcsrOutput);
		
		
		return teste;
	}

	/**
	 * @return
	 */
	@RequestMapping(value="/error",method = RequestMethod.GET)
	public Object erro(){
		return "ERRADO TUDO ERRADO";
	}
}

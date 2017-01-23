package com.craam.wellington.interfaces;

import java.util.HashMap;

import com.craam.wellington.ramaty.Ramaty;
import com.tcc.faganello.jsonOutPut.ISRCSRModel;

public interface IOutput {
	public Boolean generateOutput(IAlgorithm algorithm);
	public ISRCSRModel getResult();
}

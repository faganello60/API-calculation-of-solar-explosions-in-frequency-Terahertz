package com.tcc.faganello.jsonOutPut;

public class ISRCSRModel {
	GeneralModel compact, csr, extended, high, isr, low, total;

	public ISRCSRModel() {
		compact = new GeneralModel();
		csr =  new GeneralModel();
		extended = new GeneralModel();
		high = new GeneralModel();
		isr = new GeneralModel();
		low = new GeneralModel();
		total = new GeneralModel();
	}

	public GeneralModel getCompact() {
		return compact;
	}

	public void setCompact(GeneralModel compact) {
		this.compact = compact;
	}

	public GeneralModel getCsr() {
		return csr;
	}

	public void setCsr(GeneralModel csr) {
		this.csr = csr;
	}

	public GeneralModel getExtended() {
		return extended;
	}

	public void setExtended(GeneralModel extended) {
		this.extended = extended;
	}

	public GeneralModel getHigh() {
		return high;
	}

	public void setHigh(GeneralModel high) {
		this.high = high;
	}

	public GeneralModel getIsr() {
		return isr;
	}

	public void setIsr(GeneralModel isr) {
		this.isr = isr;
	}

	public GeneralModel getLow() {
		return low;
	}

	public void setLow(GeneralModel low) {
		this.low = low;
	}

	public GeneralModel getTotal() {
		return total;
	}

	public void setTotal(GeneralModel total) {
		this.total = total;
	}
}

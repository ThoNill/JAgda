package tho.nill.JAgda;

import java.util.ArrayList;
import java.util.List;

public class CalculationSteps extends AgdaObject {

	private List<CalculationStep> steps = new ArrayList<>();

	public CalculationSteps(String simpleName, boolean visible, AgdaModul modul) {
		super(simpleName, visible, modul);
	}

	public boolean add(CalculationStep e) {
		return steps.add(e);
	}

	public List<CalculationStep> getSteps() {
		return steps;
	}

}
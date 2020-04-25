package tho.nill.JAgda;

import java.util.ArrayList;
import java.util.List;

public class WithConstructorsType extends AgdaType {
	List<AgdaConstructor> constructors = new ArrayList<>();

	public WithConstructorsType(String simpleName, boolean visible, AgdaModul modul, AgdaType type) {
		super(simpleName, visible, modul, type);
	}

	public List<AgdaConstructor> getConstructors() {
		return constructors;
	}

	public boolean add(AgdaConstructor e) {
		if (constructors.stream().filter(x -> x.getSimpleName().equals(e.getSimpleName())).findFirst().isPresent()) {
			throw new AgdaException("es gibt schon einen Konstruktor mit dem Namen "+ e.getSimpleName());
		};
		return constructors.add(e);
	}

	@Override
	public boolean isSubtype(AgdaType type) {
		return equals(type);
	}

	@Override
	public String toString() {
		return " " + getSimpleName() + "";
	}
}
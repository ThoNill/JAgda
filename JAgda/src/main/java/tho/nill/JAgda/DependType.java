package tho.nill.JAgda;

public class DependType extends FunctionType {
	private AgdaInstance parameter;

	public DependType(String name,boolean visible, AgdaModul modul, AgdaInstance parameter, AgdaType target, AgdaType type) {
		super(name,visible, modul, parameter.getType(), target, type);
		this.parameter = parameter;
	}

	@Override
	public boolean isSubtype(AgdaType type) {
		return equals(type);
	}

	
	public AgdaInstance getParameter() {
		return parameter;
	}

	@Override
	public String toString() {
		return " " + getSimpleName() + " (" + parameter +")";
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((parameter == null) ? 0 : parameter.hashCode());
		return result;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		DependType other = (DependType) obj;
		if (parameter == null) {
			if (other.parameter != null)
				return false;
		} else if (!parameter.equals(other.parameter))
			return false;
		return true;
	}

}

package tho.nill.JAgda;

public class FunctionType extends WithConstructorsType {

	private AgdaType source;
	private AgdaType target;

	public FunctionType( String name,boolean visible, AgdaModul modul,AgdaType source, AgdaType target,AgdaType type) {
		super(name, visible, modul,type);
		this.source = source;
		this.target = target;
	}
	
	public FunctionType( boolean visible, AgdaModul modul,AgdaType source, AgdaType target,AgdaType type) {
		super(source.getSimpleName() + " -> " + target.getSimpleName(), visible, modul,type);
		this.source = source;
		this.target = target;
	}
	
	public AgdaType getSource() {
		return source;
	}
	
	public AgdaType getTarget() {
		return target;
	}

	@Override
	public boolean isSubtype(AgdaType type) {
		return target.isSubtype(type);
	}

	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((source == null) ? 0 : source.hashCode());
		result = prime * result + ((target == null) ? 0 : target.hashCode());
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
		FunctionType other = (FunctionType) obj;
		if (source == null) {
			if (other.source != null)
				return false;
		} else if (!source.equals(other.source))
			return false;
		if (target == null) {
			if (other.target != null)
				return false;
		} else if (!target.equals(other.target))
			return false;
		return true;
	}

}

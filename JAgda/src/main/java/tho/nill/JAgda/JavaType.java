package tho.nill.JAgda;

public class JavaType extends AgdaType {
	private Class<?> clazz;

	public JavaType(String simpleName, boolean visible, AgdaModul modul,Class<?> clazz) {
		super(simpleName, visible, modul,AgdaType.SET);
		this.clazz = clazz;
	}


	@Override
	public boolean isSubtype(AgdaType type) {
		if (type instanceof JavaType) {
			return this.clazz.isAssignableFrom(((JavaType)type).clazz);
		}
		return  false;
	}

	public boolean isInstance(AgdaInstance  instance) {
		return isSubtype(instance.getType()) && (instance.getInstance() == null || clazz.isInstance(instance.getInstance()));
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((clazz == null) ? 0 : clazz.hashCode());
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
		JavaType other = (JavaType) obj;
		if (clazz == null) {
			if (other.clazz != null)
				return false;
		} else if (!clazz.equals(other.clazz))
			return false;
		return true;
	}


}

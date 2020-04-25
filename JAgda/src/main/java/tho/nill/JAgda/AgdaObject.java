package tho.nill.JAgda;

public class AgdaObject {


	private String simpleName;
	private boolean visible;
	private AgdaModul modul;
	
	
	public AgdaObject(String simpleName, boolean visible, AgdaModul modul) {
		super();
		this.simpleName = simpleName;
		this.visible = visible;
		this.modul = modul;
	}
	
	public AgdaModul getModul() {
		return modul;
	}

	public String getSimpleName() {
		return simpleName;
	}
	
	public boolean isVisible() {
		return visible;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AgdaObject other = (AgdaObject) obj;
		if (modul == null) {
			if (other.modul != null)
				return false;
		} else if (!modul.equals(other.modul))
			return false;
		if (simpleName == null) {
			if (other.simpleName != null)
				return false;
		} else if (!simpleName.equals(other.simpleName))
			return false;
		if (visible != other.visible)
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((modul == null) ? 0 : modul.hashCode());
		result = prime * result + ((simpleName == null) ? 0 : simpleName.hashCode());
		result = prime * result + (visible ? 1231 : 1237);
		return result;
	}
	
	public String toString() {
		return simpleName;
	}
}

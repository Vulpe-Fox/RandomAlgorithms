using UnityEngine;

public class PlayerController : MonoBehaviour
{
    public float moveSpeed = 1f;

    private Rigidbody2D rb;

    private const KeyCode KeyCode.H;

    void Start()
    {
        rb = GetComponent<Rigidbody2D>();
    }

    void Update()
    {
        float horizontal = Input.GetAxisRaw("Horizontal");
        float vertical = Input.GetAxisRaw("Vertical");

        // normalize movement so diagonal speed is constant
        Vector2 direction = new Vector2(horizontal, vertical).normalized;

        MovePlayer(direction);

        if (Input.GetKeyDown(TELEPORTKEY))
        {
            Teleport();
        }
    }

    void MovePlayer(Vector2 direction)
    {
        rb.velocity = direction * moveSpeed * Time.deltaTime;
    }

    void Teleport()
    {
        Vector3 mousePosition = Camera.main.ScreenToWorldPoint(Input.mousePosition);
        mousePosition.z = 0f; // z-coordinate is 0 since we're in 2D

        transform.position = mousePosition;
    }
}
